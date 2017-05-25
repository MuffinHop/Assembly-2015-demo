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
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    vec3 p2=p;
    vec4 tex = texture2D(mixerTex,p.xy/28.-vec2(.5,.65));
    vec4 tex2 = texture2D(mixerTex3,vec2(atan(p.x,p.z),p.y/12.));
    finalDistance=sdSphere(p, 12.+tex.r/8.-p.z/12.);
    finalDistance=smin(finalDistance,sdSphere(p-vec3(0.,-5.,2.), 12.25), 2.3);
    finalDistance=smin(finalDistance,sdSphere(p-vec3(0.,-10.,-1.), 9.), 2.3);

    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                        (p-vec3(5.,0.,-12.+abs(p.x/2.)))*rotationMatrix(vec3(1.,0.,0.),3.14159/2.), 
                        vec2(3.5,2.)) );
    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                        (p-vec3(-5.,0.,-12.+abs(p.x/2.)))*rotationMatrix(vec3(1.,0.,0.),3.14159/2.), 
                        vec2(3.5,2.)) );
    finalDistance=max(
                    finalDistance, 
                    -sdCappedCylinder(
                        (p-vec3(5.,0.,-14.+abs(p.x/2.)))*rotationMatrix(vec3(1.,0.,0.),3.14159/2.), 
                        vec2(3.,2.)) );
    finalDistance=max(
                    finalDistance, 
                    -sdCappedCylinder(
                        (p-vec3(-5.,0.,-14.+abs(p.x/2.)))*rotationMatrix(vec3(1.,0.,0.),3.14159/2.), 
                        vec2(3.,2.)) );

    finalDistance=smin(
                    finalDistance, 
                    sdSphere(p-vec3(0.,-11.,-9.),6.), 1.1 );

    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                    (p-vec3(0.,-9.5,-14.))*rotationMatrix(vec3(1.-p.z*11.,0.,0.),3.14159/2.+.2),vec2(5.5-abs(p.z)/22.,2.)));
    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                    (p-vec3(0.,-8.5,-3.))*rotationMatrix(vec3(1.-p.z*11.,0.,0.),3.14159/2.+.2),vec2(4.,15.)));

    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                    (p-vec3(7.,-9.5,-14.))*rotationMatrix(vec3(0.,0.,0.5),3.14159/2.+.2),vec2(5.5-abs(p.x)/8.,2.)));
    finalDistance=min(
                    finalDistance, 
                    sdCappedCylinder(
                    (p-vec3(-7.,-9.5,-14.))*rotationMatrix(vec3(0.,0.,-0.5),3.14159/2.+.2),vec2(5.5-abs(p.x)/8.,2.)));

    finalDistance-=(-tex.r+tex2.r)/24.;
    color=tex*(tex+vec4(.6,.34,.3,.4));

    float ballo=1000000.;
    for(float i=0.; i<5.; i++)
        ballo=min(ballo, sdBox(
                (p - 
                vec3(
                sin(i+iGlobalTime/8.),
                cos(i*2.2+iGlobalTime*1.3),
                sin(i*2.2+iGlobalTime*1.3))*25.)*rotationMatrix(vec3(0.,0.,-0.5),3.14159/2.+.2+i), vec3(5.)));
    if(ballo<finalDistance) {
        finalDistance=ballo;
        color.rgb=vec3(1.,0.5,0.2);
        color.a=1.;
    }
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 10000000.;
    
    color = vec4(0.0);
    reflectiveValue = 2.2;
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
    return clamp(res,0.1,9.0);
}
vec4 tracer(vec3 rayStartPosition, vec3 rayD) {
    const float epsilon = 1./256.;
    vec3 rayDirection=rayD;
    vec3 rayPosition = rayStartPosition + cameraMove*iGlobalTime;

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
     for(float k=0.; k<2.; k++) {
        for(float i=0.; i<28.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            if(length(rayPosition)>40.-k*12.) break;
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
    vec2 postex=vec2(atan(rayDirection.x,rayDirection.z)/3.-.4,-rayDirection.y/2.-.5);
    returnColor.rgb+=sin(rayDirection.y*2.+3.14159/2.)*pow(texture2D(mixerTex3,postex).rgb,vec3(2.))/(collisions*collisions)*(1.-abs(rayDirection.y)*3.);
    return returnColor;
}
vec4 piip() {
    vec3 cameraPosition = cameraStart;
    cameraPosition+=vec3(0.,4.2,0.7)*iGlobalTime;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
    vec3 direction = normalize(vec3(.5 * uv * vec2(aspect, 1.0), 1. ));
    vec3 camRot = cameraRotateStart+vec3(0.2,0.02,0.)*iGlobalTime;
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