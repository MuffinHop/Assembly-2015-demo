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

#define ST_T(X) (X).x
#define ST_SIGN(X) ((X).y < 0 ? -1 : +1)
#define ST_ABS_RADIUS(X) abs((X).y)
#define ST_RADIUS(X) (X).y
#define ST_HIT(X) ((X).x != INFINITY)

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
float cell( vec3 p, vec2 h) {
    float r=sdHexPrism( p, h );
    r=max(r,-sdHexPrism( p, vec2(h.x-.3, h.y+1.) ));
    return r;
}

void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    float t=iGlobalTime/2.;
    p.z+=t*70.;
    p.x+=sin(p.z/14.)*1.;
    p.y+=cos(p.z/14.)*1.;
    p.xy*=1.+sin(p.z/32.)/4.;
    p.xy+=vec2(sin(t),cos(t))*8.;
    p*=rotationMatrix(vec3(0.,0.,1.),t/3.);
    vec3 tex=texture2D(mixerTex,vec2(atan(p.x,p.y),p.z/8.)).rgb;

    finalDistance=min(finalDistance, cell(p, vec2(12.+tex.r/12., 11111.)));
    
    p*=rotationMatrix(vec3(0.,0.,1.),sin(length(p.xy)/12.+p.z/12.));
    p.x=mod(p.x,13.5);
    p.y=mod(p.y,8.)-4.0;
    p.z=mod(p.z,32.)-16.0;
    finalDistance=min(finalDistance, cell(p-vec3(4.5,0.,0.), vec2(4., 2.)));
    finalDistance=min(finalDistance, cell(p-vec3(11.2,4.,0.), vec2(4., 2.)));
    finalDistance=min(finalDistance, cell(p-vec3(11.2,-4.,0.), vec2(4., 2.)));
    finalDistance=min(finalDistance, cell(p-vec3(-2.2,4.,0.), vec2(4., 2.)));
    finalDistance=min(finalDistance, cell(p-vec3(-2.2,-4.,0.), vec2(4., 2.)));
    
    color.rgb=vec3(.7,floor(mod(p.z/7.,2.)),.7)/(1.+abs(p.z)/66.);
    color.a=color.g/10.;
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 1e8;
    color = vec4(0.0);
    reflectiveValue = 13.2;
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

float checkIfCloseBy(vec3 p, vec3 d) {
    float epsilon = 1./1024.;
    float finalDistance = 1e8;
    vec4 color = vec4(0.0);
    float reflectiveValue = 0.0;
    for(float i=0.; i<16.; i++) {
        p+=d+vec3(rand(i)-.5,rand(i+11.)-.5,rand(i+11.)-.5)*epsilon/2.;
        objectGroup0Light(p, finalDistance, color, reflectiveValue);
        epsilon/=2.;
    }
    return epsilon;
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

vec4 tracer(vec3 rayStartPosition, vec3 rayDirection) {
    const float epsilon = 1./256.;
    
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
        for(float i=0.; i<22.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=0.001/(1.+stepable/2.);
            if(length(rayPosition)>88.) break;
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
    return returnColor-vec4(vec3(fog),1.);
}
vec4 piip() {
    vec3 cameraPosition = cameraStart;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    
    float aspect = iResolution.x / iResolution.y;
    vec3 direction = normalize(vec3(.5 * uv * vec2(aspect, 1.0), 1. ));
    

    direction *= rotationMatrix(vec3(1.,0.,0.),cameraRotateStart.x);
    cameraPosition *= rotationMatrix(vec3(1.,0.,0.),cameraRotateStart.x);
    direction *= rotationMatrix(vec3(0.,1.,0.),cameraRotateStart.y);
    cameraPosition *= rotationMatrix(vec3(0.,1.,0.),cameraRotateStart.y);
    direction *= rotationMatrix(vec3(0.,0.,1.),cameraRotateStart.z);
    cameraPosition *= rotationMatrix(vec3(0.,0.,1.),cameraRotateStart.z);


    return tracer(cameraPosition, direction);
}

void main(){
    
    vec2 p = gl_FragCoord.xy / iResolution.xy;
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
    float grid=0.;
    if(mod(uv.x*800.,16.)==0.)
        grid=1.;
    if(mod(uv.y*800.,16.)==0.)
        grid=1.;
        gl_FragColor = piip()/(1.+grid*12.);
}