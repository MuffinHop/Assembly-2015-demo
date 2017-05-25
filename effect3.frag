precision lowp float;
uniform sampler2D mixerTex;
uniform sampler2D mixerTex2;
uniform sampler2D mixerTex3;
uniform float iGlobalTime;
uniform vec2 iResolution;
uniform vec3 light;
uniform float lightPower;
uniform float lightSize;
uniform float shadowItr;
uniform float marchItr;
uniform vec3 cameraStart;
uniform vec3 cameraRotateStart;
uniform vec3 cameraRotateMove;
uniform vec3 cameraMove;
uniform float fogify;
uniform float wavey;

float rand(vec2 co) {
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float rand(float c){
    return rand(vec2(c));
}
float smin( float a, float b, float k ) {
    float res = exp( -k*a ) + exp( -k*b );
    return -log( res )/k;
}
float smax( float a, float b, float k ) {
    float res = exp( k*a ) + exp( k*b );
    return log( res )/k;
}
mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    
    return mat3(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c);
}
float udRoundBox( vec3 p, vec3 b, float r ){
  return length(max(abs(p)-b,0.0))-r;
}
float sdSphere( vec3 p, float s ){
  return length(p)-s;
}
float sdBox( vec3 p, vec3 b ){
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}
float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}
float sdPlane( vec3 p, vec4 n ) {
  return dot(p,n.xyz) + n.w;
}
float sdTriPrism( vec3 p, vec2 h )
{
    vec3 q = abs(p);
    return max(q.z-h.y,max(q.x*0.866025+p.y*0.5,-p.y)-h.x*0.5);
}
float sdHexPrism( vec3 p, vec2 h )
{
    vec3 q = abs(p);
    return max(q.z-h.y,max((q.x*0.866025+q.y*0.5),q.y)-h.x);
}
float sdCappedCylinder( vec3 p, vec2 h )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}
float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}
float blobs(vec3 p, out vec4 c) {
    float distance=1e8;
    float plooh = wavey*sin(iGlobalTime+p.y*2.+mod(iGlobalTime,1.)/11.) ;
    distance = sdBox((p-vec3(0.,5.,texture2D(mixerTex2, p.xz/9.).r*0.01)), vec3(plooh+  3.+floor(mod(p.y*3.,2.))/32.-p.y/2. ,3.,plooh+  3.+floor(mod(p.y*3.,2.))/32.-p.y/2.  ) );
    distance+=texture2D(mixerTex2, p.xz/12.).r*0.01*texture2D(mixerTex2, vec2(0.4,-0.6)*p.xz-vec2(.5,.2)).r;
    c.rgb=(floor(p.y)/10.+vec3(.1,.04,.04))*(floor(mod(p.y*3.,2.))*2.+0.1)*texture2D(mixerTex2, vec2(0.4,-0.6)*p.xz-vec2(.5,.2)).r*2.4;
    vec3 p2=p;
    p2.x=mod(p2.x,0.2)-0.1;
    p2.z=mod(p2.z,0.2)-0.1;
    distance = max(distance, -sdBox(p2, vec3(.08+rand(floor(p2.zy))/24.,1.,.08+rand(floor(p2.xy))/16.)));
    return distance;
}
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    finalDistance=blobs(p, color);
    reflectiveValue=0.2;

    p.y+=length(p.xz)/8.;
    vec3 p2=p;
    p2.x=mod(p2.x,2.)-1.;
    p2.y=mod(p2.y,2.)-1.;
    p2.z=mod(p2.z,2.)-1.;

    vec3 tex=texture2D(mixerTex, -iGlobalTime/555.+p.xz/6.).rgb/2.+texture2D(mixerTex, iGlobalTime/444.+p.xz/8.).rgb/2.;
    float h=(sin(p.x/4.)+sin(p.z/1.3)+sin(p.x/2.41))/7.+sin(p.x/9.)+sin(p.z/6.3)+sin(p.x/7.31);
    h/=3.6;
    float c1 = sdBox(p,vec3(1000.,1.7+h,1000.));
    c1 = smin(c1,sdSphere(p,4.4),1.7);
    c1 = smax(c1,-sdSphere(p-vec3(0.,3.,0.),2.),1.9);
    c1 = smax(c1,-sdSphere(p2,0.8+sin(p.x+p.z+iGlobalTime/11.)/4.),1.5);
    c1 -= tex.r/4.;

    if(c1<finalDistance) {
        finalDistance=c1;
        color.rgb=tex*vec3(0.4,0.2,0.2)/(0.5+length(p.xz)/8.);
        color.a=0.2;
        reflectiveValue=0.7+tex.r;
    }
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 1e8;
    color = vec4(0.0);
    reflectiveValue = 0.2;
    objectGroup0Light(p, finalDistance, color, reflectiveValue) ;
    return finalDistance;
}

float calculateDistanceLight(in vec3 p) {
    float finalDistance = 10000000.;
    vec4 dummy=vec4(0.);
    float reflectiveValue = 0.0;
    objectGroup0Light(p, finalDistance, dummy, reflectiveValue);
    return finalDistance;
}



float traceToLight(vec3 rayPosition, vec3 normalVector, vec3 lightSource){
    vec3 ro = rayPosition + normalVector/128.;
    vec3 rd = normalize(lightSource - rayPosition);
    float t = 0.01;
    float k = 1.6;
    float res = 1.0;
    for( int i=0; i<22; i++ )
    {
        float h = calculateDistanceLight(ro + rd*t);
        h = max( h, 0.0 );
        res = min( res, k*h/t );
        t += clamp( h, 0.001, 0.9 );
        if( h<1./512.) break;
    }
    return clamp(res,0.0,9.0);
}
vec4 tracer(vec3 rayStartPosition, vec3 rayDirection) {
    const float epsilon = 1./512.;
    
    vec3 rayPosition = rayStartPosition;

    vec3 normalVector;
    float dist = 0.0;
    vec4 returnColor = vec4(0.0);
    vec4 finalColor = vec4(0.0);
    vec3 lightSource = light;
    vec3 lightColor = vec3(1.0);
    float reflectiveValue = 1.0;
    float reflectionNow = 100.0;
    float finalLight = 1.0;
    vec4 endlighted = vec4(0.);
    float possibleFogLight = 1.0;
    float firstBunchDist=100000.;
    float collisions=1.;
    float fog=0.;
    for(float k=0.; k<2.; k++) {
        for(float i=0.; i<44.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=fogify/(1.+stepable/2.+length(rayPosition)/7.);
            if(length(rayPosition.xz)>10.) break;
            if( abs(stepable) <= epsilon){
                collisions++;
                vec3 C;
                float dummy = 0.0;
                normalVector = vec3(    calculateDistanceLight(rayPosition+vec3(epsilon,0,0))-calculateDistanceLight(rayPosition+vec3(-epsilon,0,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,epsilon,0))-calculateDistanceLight(rayPosition+vec3(0,-epsilon,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,0,epsilon))-calculateDistanceLight(rayPosition+vec3(0,0,-epsilon)));
                normalVector = normalize(normalVector);
                float light = traceToLight(rayPosition, normalVector, lightSource);
                finalLight = max(min(finalLight, light),.05);
                float lightDistance = distance(rayStartPosition,lightSource);
                
                finalColor = color * vec4(vec3(dot(normalVector, -rayDirection)), 1.);
                
                
                vec3 lightDir = (lightSource-rayPosition);
                lightDir = normalize(lightDir);
                float directLight = dot(normalVector, lightDir);
                
                
                returnColor += vec4( vec3(finalLight*max(lightSize*pow(directLight,lightPower)*lightColor,0.05)) / (k*2.8/reflectionNow + 1.0), 1.) * finalColor;
                reflectionNow = min(reflectionNow,reflectiveValue);
  
                    
                firstBunchDist = i;
                break;
                
            }
            returnColor.rgb+=dist/28000.;
            if(k==0. && collisions==0.) break;
                if(k==0.) {
                    vec3 lightDir = (lightSource-rayStartPosition);
                    lightDir = normalize(lightDir);
                    float directLight = dot(rayDirection, lightDir);
                    endlighted.rgb=min(max( pow(directLight,lightPower) * lightColor * 0.4, 0.01),1.);
                }
        }
        dist = 0.01;
        rayStartPosition = rayPosition + normalVector;
        rayPosition = rayStartPosition;
        rayDirection = reflect(rayDirection, normalVector);
    } 
    for(float i=0.; i<10.; i++) {
    vec2 postex=vec2(atan(rayDirection.x,rayDirection.z)/3.-.4,-rayDirection.y/3.-.5+(rand(i)+iGlobalTime*0.5)/100.);
        returnColor.rgb+=0.05*abs(rayDirection.y*2.+3.14159/2.)*pow(texture2D(mixerTex3,postex).rgb,vec3(2.))/(collisions*collisions*collisions)*(1.-abs(rayDirection.y)*3.);
    }
    for(float i=0.; i<10.; i++) {
    vec2 postex=vec2(atan(rayDirection.x,rayDirection.z)/3.-.4,-rayDirection.y/3.-.44+(rand(i)+iGlobalTime*0.3)/100.);
        returnColor.rgb+=0.05*abs(rayDirection.y*2.+3.14159/2.)*pow(texture2D(mixerTex3,postex).rgb,vec3(2.))/(collisions*collisions*collisions)*(1.-abs(rayDirection.y)*3.);
    }
    return returnColor+vec4(vec3(fog),1.);
}
vec4 piip() {
    vec3 cameraPosition = cameraStart;
    cameraPosition+=cameraMove*iGlobalTime;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    
    float aspect = iResolution.x / iResolution.y;
    vec3 direction = normalize(vec3(.5 * uv * vec2(aspect, 1.0), 1. ));
    vec3 camRot = cameraRotateStart+cameraRotateMove*iGlobalTime;
    direction *= rotationMatrix(vec3(1.,0.,0.),camRot.x);
    direction *= rotationMatrix(vec3(0.,1.,0.),camRot.y);
    direction *= rotationMatrix(vec3(0.,0.,1.),camRot.z);
    return tracer(cameraPosition, direction);
}

void main(){
    
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
        gl_FragColor = piip();
}