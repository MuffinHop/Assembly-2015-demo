precision lowp float;
uniform sampler2D mixerTex;
uniform sampler2D mixerTex2;
uniform sampler2D mixerTex3;
uniform float iGlobalTime;
uniform float iLocalTime;
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
uniform vec3 colorlight;
uniform vec3 shadowcolor;
uniform float mushi;

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
float sdCone( vec3 p, vec2 c )
{
    // c must be normalized
    float q = length(p.xy);
    return dot(c,vec2(q,p.z));
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
mat2 rotate(float Angle){
    mat2 rotation = mat2(
        vec2( cos(Angle),  sin(Angle)),
        vec2(-sin(Angle),  cos(Angle))
    );
    return rotation;
}
float roundBox(vec2 coord, vec2 pos, vec2 b, float c ){
  return 1.-floor(length(max(abs(coord-pos)-b,c)));
}
float circle(vec2 coord, vec2 pos, float size){
    return min(floor(distance(coord,pos)-size),0.);
}
float sdCapsule( vec2 p, vec2 a, vec2 b, float r ){
    vec2 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return min(floor(length( pa - ba*h ) - r),0.);
}
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){

    for(float i=1.; i<5.; i++) {
        vec3 toppos=vec3(
                        3.*(i-2.),
                        min(-4.+iLocalTime*(rand(i+mushi)/3.+.7),4.+rand(i+mushi)*2.),
                        -8.+rand(i+44.+mushi)*6.);
        vec3 bottompos=vec3(3.*(i-2.),-4.,-8.+rand(i+44.)*4.);

        vec3 magicdistort=vec3(sin(i*55.+p.y/3.),0.,cos(i*55.+p.y/3.6));
        vec3 pmush = p;
       //pmush*=1.-p.y/12.;

        //finalDistance = min( finalDistance, sdSphere( pmush - toppos - vec3(0.,0.2,0.) - magicdistort, 1.2) );
       // finalDistance = max( finalDistance, -sdSphere( pmush - toppos - vec3(0.,-0.2,0.) - magicdistort, 1.2) );
        finalDistance = min( finalDistance,
                                sdCapsule(
                                    pmush - magicdistort,
                                    toppos,
                                    toppos-vec3(0.,-.2,0.),0.7+min(-4.+iLocalTime*(rand(i)/3.+.7)/(1.+iLocalTime/12.),1.) ) );
        finalDistance = max( finalDistance,
                                -sdCapsule(
                                    pmush - magicdistort,
                                    toppos,
                                    toppos-vec3(0.,2.2,0.),0.5+min(-4.+iLocalTime*(rand(i)/3.+.7)/(1.+iLocalTime/12.),1.) ) );



        finalDistance = min( finalDistance,
                                sdCapsule(
                                    pmush - magicdistort,
                                    toppos,
                                    bottompos,0.4+0.7/(1.+distance(p,bottompos))) );

        finalDistance -=texture2D(mixerTex,(p.xy)/vec2(8.)-vec2(.5)).r/32.;

    }

    color.rgb=texture2D(mixerTex,(p.xy)/vec2(8.)).rgb*(vec3(1.)+vec3(sin(p.x)/12.+.3,cos(p.z)/12.+.3,0.5));
    reflectiveValue=1.2;
    float ceiling=sdBox(p-vec3(0.,0.,9.),vec3(100.,100.,10.));
    ceiling = min( ceiling,
                         sdBox(
                         p-vec3(0.,-5.,0.),
                         vec3(100.,1.+texture2D(mixerTex,p.xz/5.).r/12.,100.)));
    if(ceiling<finalDistance) {
        finalDistance=ceiling;
        vec2 tp=vec2(1.,-1.)*p.xy/12.+vec2(0.5);
        tp.x=min(max(tp.x,0.),1.);
        tp.y=min(max(tp.y,0.),1.);
        color.rgb=texture2D(mixerTex, p.xz/2.).rgb/4.+vec3(floor(mod(p.y,2.)))*vec3(.1);
        reflectiveValue=3.1;
    }


}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 1e8;
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
    for( int i=0; i<29; i++ )
    {
        float h = calculateDistanceLight(ro + rd*t);
        h = max( h, 0.0 );
        res = min( res, k*h/t );
        t += clamp( h, 0.001, 0.9 );
        if( h<1./900.) break;
    }
    return clamp(res,0.0,9.0);
}
vec4 tracer(vec3 rayStartPosition, vec3 rayDirection) {
    const float epsilon = 1./160.;
    
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
        for(float i=0.; i<30.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=0.001/(1.+stepable/2.);
            if(length(rayPosition)>18.) break;
            if( abs(stepable) <= epsilon){
                collisions++;
                vec3 C;
                float dummy = 0.0;
                normalVector = vec3(    calculateDistanceLight(rayPosition+vec3(epsilon,0,0))-calculateDistanceLight(rayPosition+vec3(-epsilon,0,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,epsilon,0))-calculateDistanceLight(rayPosition+vec3(0,-epsilon,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,0,epsilon))-calculateDistanceLight(rayPosition+vec3(0,0,-epsilon)));
                normalVector = normalize(normalVector);
                float light = traceToLight(rayPosition, normalVector, lightSource);
                finalLight = max(min(finalLight, light),.02);
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
    return returnColor-vec4(vec3(fog),1.);
}
vec4 piip() {
    vec3 cameraPosition = cameraStart;
    cameraPosition+=cameraMove*iLocalTime;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    //if(uv.x*uv.y< 0.) discard;
    float aspect = iResolution.x / iResolution.y;
    vec3 direction = normalize(vec3(.5 * uv * vec2(aspect, 1.0), 1. ));
    vec3 camRot = cameraRotateStart+cameraRotateMove*iLocalTime;
    direction *= rotationMatrix(vec3(1.,0.,0.),camRot.x);
    direction *= rotationMatrix(vec3(0.,1.,0.),camRot.y);
    direction *= rotationMatrix(vec3(0.,0.,1.),camRot.z);
    return tracer(cameraPosition, direction);
}

void main(){
    
    vec2 p = gl_FragCoord.xy / iResolution.xy;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
    vec4 peep=piip();

        gl_FragColor = peep;
}