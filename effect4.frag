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

float length2( vec2 p ) {
    return sqrt(p.x * p.x + p.y * p.y);
}
 
float length6( vec2 p ) {
    p=p * p * p;
    p=p * p;
    return pow(p.x + p.y, 1.0 / 6.0);
}
 
float length8( vec2 p ) {
    p=p * p;
    p=p * p;
    p=p * p;
    return pow(p.x + p.y, 1.0 / 8.0);
}
float sdTorus82( vec3 p, vec2 t )
{
  vec2 q = vec2(length2(p.xz)-t.x,p.y);
  return length8(q)-t.y;
}
float blobs(vec3 p, out vec4 c) {
    float distance=1e8;
    distance = sdBox((p-vec3(0.,5.,0.)), vec3(3.+floor(mod(p.y*3.,2.))/32.-p.y/2.,3.,3.+floor(mod(p.y*3.,2.))/32.-p.y/2.) );
    distance-=texture2D(mixerTex2, p.xz/7.).r*0.03;
    c.rgb=vec3(.82,.23,.6)*(floor(mod(p.y*3.,2.))+0.4);
    vec3 p2=p;
    p2.x=mod(p2.x,0.2)-0.1;
    p2.z=mod(p2.z,0.2)-0.1;
    distance = max(distance, -sdBox(p2, vec3(.08+rand(floor(p2.zy))/32.,1.,.08+rand(floor(p2.xy))/32.)));
    return distance;
}
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    finalDistance=blobs(p, color);
    reflectiveValue=1.6;
    float dist0=1e8;
    dist0=sdBox(p-vec3(0.,-3.,0.),vec3(100.,4.-sin(rand(floor(p.xz*3.))*3.141*2.+iGlobalTime)/12.,100.));
    if(finalDistance>dist0) {
        finalDistance=dist0;
        color.rgb=vec3(1.1);
        reflectiveValue=0.6;
    }
    float dista=1e8;
    vec3 p2=p;
    p2.x=abs(p2.x);
    dista=min(dista,sdBox(p2-vec3(11.,0.,0.),vec3(1.+floor(max(mod(p.y*1.8,5.)-3.,0.))/16.,17.,66.)));
    vec3 p3=p;
    p3.y=mod(p.y,5.)-2.5;
    p3.z=mod(p.z,5.)-2.5;
    p3.x=abs(p.x);
    dista=min(dista,sdBox(p3-vec3(8.,0.,0.), vec3(4.0,0.5,1.5)));


    dista=min(dista,sdCappedCylinder(p2-vec3(7.,0.,0.),vec2(2.+floor(max(mod(p.y*1.8,5.)-3.,0.))/16.,12.)));
    if(finalDistance>dista) {
        finalDistance=dista;
        color.rgb=vec3(0.1)+floor(max(mod(p.y*1.8,5.)-3.,0.))*vec3(2.,0.7,0.2);
        reflectiveValue=0.6;
    }



    float distb=1e8;

    for(float i=0.; i<5.; i++) {
        distb=min(distb, sdHexPrism(p-vec3(3.*(rand(i+5.+floor(iGlobalTime/4.)-1.)-.5),5.+3.*(rand(i+4.+floor(iGlobalTime/2.)-1.)-.5),3.*(rand(i+8.+floor(iGlobalTime/4.)-1.)-.5)),vec2(2.,2.)));
        distb=max(distb, -sdHexPrism(p-vec3(3.*(rand(i+5.+floor(iGlobalTime/4.)-1.)-.5),5.+3.*(rand(i+4.+floor(iGlobalTime/2.)-1.)-.5),3.*(rand(i+8.+floor(iGlobalTime/4.)-1.)-.5)),vec2(1.8,3.)));
    }
    if(finalDistance>distb) {
        finalDistance=distb;
        color.rgb=vec3(1.,0.,0.);
        reflectiveValue=0.6;
    }
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 1e8;
    color = vec4(0.0);
    reflectiveValue = 1.2;
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
    const float epsilon = 1./1024.;
    
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
        for(float i=0.; i<66.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=fogify/(1.+stepable/2.+length(rayPosition)/7.);
            if(length(rayPosition)>44.) break;
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