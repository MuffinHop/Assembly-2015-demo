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
uniform float gamma;
uniform float ballspace;
uniform float iLocalTime;

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
float blobs(vec3 p, out vec4 c, out float reflectiveValue) {
    c.rgb=vec3(1.);
    float distance=1e8;
    p.y+=abs(p.z/12.);
   // p*=rotationMatrix(vec3(0.1,0.0,0.1),sin(p.z/22.+iGlobalTime/4.)/12.);
    vec3 p2=p;
    p2.x=abs(p2.x);
    p2.x*=1.+floor(mod((iGlobalTime+1.)/2.,2.))/8.;
    p2.z=mod(p.z,16.)-8.;
    p2.y=mod(p.y,2.)-1.;
    p2.x+=sin(rand(floor(p.y/2.)+floor(p.z/8.))*3.141*12.+iGlobalTime);
    distance=sdBox(p2-vec3(12.+3.,0.,0.), vec3(1.,1.,8.));
    distance=max(distance, -sdBox(p2-vec3(12.+1.02,0.,0.), vec3(1.2,0.7,7.8)));

    distance=min(distance, sdBox(p-vec3(0.,-5.,0.), vec3(100.,1.+texture2D(mixerTex, p.xz/19.).r/44.,100.)));
        //c.rgb=2.*vec3(0.7,3.1,3.9)*(.02+2.*floor(mod(p.z/2.2+iGlobalTime/4.,2.)))*(min(length(p)/2.,1.));
        //reflectiveValue = 0.1;
        float distball=sdSphere(p-vec3(0.,ballspace*iLocalTime*iLocalTime+1.,0.), 4.);
        for(float i=0.; i<4.; i++) {
            vec3 posiball = vec3(0.,ballspace*iLocalTime*iLocalTime+1.,0.) + 
                            3.8 * 
                                vec3(
                                    sin(iGlobalTime/5.+rand(i)*3.1415*2.),
                                    0.,
                                    cos(iGlobalTime/5.+rand(i)*3.1415*2.)) * 
                                rotationMatrix(
                                    vec3(
                                        rand(i),
                                        rand(i+22.),
                                        rand(i+33.)),
                                    3. * 
                                    ((rand(i)-.5)+1.1) * 
                                    iGlobalTime/5.);
            vec3 pball=p-posiball;
            distball = min(distball, sdSphere(pball,1.3+sin(2.*rand(i)*3.141)));
        }
            distance = min(distance, distball);
            c.rgb=vec3(.1)+2.*vec3(0.7,3.1,3.9)*(.02+2.*floor(mod(p.z/2.2+iGlobalTime/4.,2.)))*floor(min(length(p-vec3(0.,5.,0.))/6.,1.5));
            reflectiveValue=0.2;
    return distance;
}
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    finalDistance=blobs(p, color,  reflectiveValue);
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 1e8;
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
    for( int i=0; i<32; i++ )
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
    const float epsilon = 1./32.;
    
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
    for(float k=0.; k<3.; k++) {
        for(float i=0.; i<28.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=fogify/(1.+stepable/2.+length(rayPosition)/7.);
            if(abs(rayPosition.z)>13.) break;
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
                
                returnColor += vec4( vec3(finalLight*max(lightSize*pow(directLight,lightPower)*lightColor,0.1)) / (k*2.8/reflectionNow + 1.0), 1.) * finalColor;
                reflectionNow = min(reflectionNow,reflectiveValue);
  
                    
                firstBunchDist = i;
                break;
                
            }
        }
        dist = 0.08;
        rayStartPosition = rayPosition + normalVector;
        rayPosition = rayStartPosition;
        rayDirection = reflect(rayDirection, normalVector);
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
    cameraPosition*=rotationMatrix(vec3(0.,1.,0.), sin(iGlobalTime/14.)/3.);
    direction*=rotationMatrix(vec3(0.,1.,0.), sin(iGlobalTime/14.)/3.);
    return tracer(cameraPosition, direction);
}

void main(){
    
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
    vec4 peep=piip();
    vec4 endresult = peep;
    gl_FragColor = endresult;
}