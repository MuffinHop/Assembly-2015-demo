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
uniform float swing;
uniform float swingincrease;
uniform float movingspeed;
uniform float movingacceleration;

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
float ship( vec3 p) {
    if(length(p)>18.) return 10000000.;
    float dist=udRoundBox((p-vec3(8.,0.,0.))*rotationMatrix(vec3(0.,0.,1.),0.2),vec3(2.-p.z/24.,2.-p.z/24.,12.+p.y), 0.7);
    dist=min( dist, udRoundBox((p-vec3(-8.,0.,0.))*rotationMatrix(vec3(0.,0.,1.),-0.2),vec3(2.-p.z/24.,1.4-p.z/24.,12.+p.y), 0.7));

    dist=min( dist, udRoundBox((p-vec3(10.,0.7,-7.))*rotationMatrix(vec3(0.,0.,1.),0.2),vec3(1.4-p.z/24.-p.y/8.,.6-p.z/24.+p.y/4.,6.+p.y), 0.7));
    dist=min( dist, udRoundBox((p-vec3(-10.,0.7,-7.))*rotationMatrix(vec3(0.,0.,1.),-0.2),vec3(1.4-p.z/24.-p.y/8.,.6-p.z/24.+p.y/4.,6.+p.y), 0.7));

    dist=min( dist, udRoundBox((p-vec3(0.,2.,-1.))*rotationMatrix(vec3(1.,0.,0.),-0.2), vec3(4.-(abs(p.z+6.))/4.,2.-(abs(p.z+6.))/4.+sin(p.x/2.+3.141/2.),8.), 0.7));

    dist=min( dist, udRoundBox((p-vec3(0.,1.+sin(p.x/2.+3.141/2.),-6.))*rotationMatrix(vec3(1.,0.,0.),-0.2), vec3(8.,.5,.5), 0.7));

    return dist;
}
void objectGroup0Light(in vec3 p, out float finalDistance, out vec4 color, out float reflectiveValue){
    p*=rotationMatrix(vec3(0.,0.,1.),(swing+swingincrease*iLocalTime)*sin(iLocalTime+p.z/64.)/2.);
    vec3 rp=p;
    rp*=rotationMatrix(vec3(1.,0.,0.),(swing+swingincrease*iLocalTime)*sin(iLocalTime)/15.);
    rp*=rotationMatrix(vec3(0.,0.,1.),(swing+swingincrease*iLocalTime)*sin(iLocalTime*1.4)/18.);
    vec3 crp=rp;
    rp.x+=sin(iLocalTime)*min(16.*(movingspeed+(movingacceleration*iLocalTime)),16.);
    float dist0=ship(rp);
    p.z+=iLocalTime*100.*(movingspeed+(movingacceleration*iLocalTime));
        vec3 tex=texture2D(mixerTex,rp.xz/16.).rgb;
        vec3 tex2=texture2D(mixerTex2,rp.zx/13.).rgb;
    //p.z+=iGlobalTime*64.;
    //p.x+=sin(p.z/24.)*12.+sin(p.z/44.)*12.;
    finalDistance=min(finalDistance, sdBox( p-vec3(0.,-8.,0.), vec3(54.,2.,111111.)));
    finalDistance=max(finalDistance, -sdBox( p-vec3(0.,-6.,0.), vec3(48.,3.6+floor(mod(p.x/8.,2.))*floor(mod(p.z/8.,2.))/3.,111111.)));
    vec3 p2 = vec3(mod(p.x,128.)-64., p.y, mod(p.z,48.)-24.);
    finalDistance=min(finalDistance, sdBox( p2, vec3(14.,55.,8.)));
    color.rgb=vec3(floor(mod(p.z/19.,2.))/2.+.7)*(0.5+floor(mod(p.x/8.,2.))*floor(mod(p.z/8.,2.))/2.);
    reflectiveValue=7.;
    if(dist0<finalDistance){
        finalDistance=dist0+tex2.r/6.+tex.r/64.;
        color.rgb= tex2;
        reflectiveValue=3.;
    }
    
}

float calculateDistance(in vec3 p, out vec4 color, out float reflectiveValue) {
    float finalDistance = 10000000.;
    
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
    return clamp(res,0.02,9.0);
}
vec4 tracer(vec3 rayStartPosition, vec3 rayDirection) {
    const float epsilon = 1./256.;
    
    vec3 rayPosition = rayStartPosition;

    vec3 normalVector;
    float dist = 0.0;
    vec4 returnColor = vec4(0.0);
    vec4 finalColor = vec4(0.0);
    vec3 lightSource = vec3(10.,50.,mod(iGlobalTime*4.,50.)-25.);
    float lp=lightPower+mod(iGlobalTime*15.,2.);
    vec3 lightColor = vec3(1.1,1.0,1.0) * 2.;
    float reflectiveValue = 1.0;
    float reflectionNow = 100.0;
    float finalLight = 1.0;
    vec4 endlighted = vec4(0.);;
    float possibleFogLight = 1.0;
    float firstBunchDist=100000.;
    float fog=0.;
    int col=0;
    for(float k=0.; k<2.; k++) {
        for(float i=0.; i<55.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            fog+=0.00008*abs(rayPosition.z);
            if(length(rayPosition.xy)>55.) break;
            if(abs(rayPosition.z)>80.) break;
            if( abs(stepable) <= epsilon){
                vec3 C;
                float dummy = 0.0;
                normalVector = vec3(    calculateDistanceLight(rayPosition+vec3(epsilon,0,0))-calculateDistanceLight(rayPosition+vec3(-epsilon,0,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,epsilon,0))-calculateDistanceLight(rayPosition+vec3(0,-epsilon,0)),
                                        calculateDistanceLight(rayPosition+vec3(0,0,epsilon))-calculateDistanceLight(rayPosition+vec3(0,0,-epsilon)));
                normalVector = normalize(normalVector);
                float light = traceToLight(rayPosition, normalVector, lightSource);
                finalLight = min(finalLight, light);
                float lightDistance = distance(rayStartPosition,lightSource);
                
                finalColor = color * vec4(vec3(dot(normalVector, -rayDirection)), 1.);
                
                
                vec3 lightDir = (lightSource-rayPosition);
                lightDir = normalize(lightDir);
                float directLight = dot(normalVector, lightDir);
                
                
                returnColor += vec4( vec3(finalLight*max(lightSize*pow(directLight,lp)*lightColor,0.05)) / (k*2.8/reflectionNow + 1.0), 1.) * finalColor;
                reflectionNow = min(reflectionNow,reflectiveValue);
  
                col=1;
                firstBunchDist = i;
                break;
                
            }
        }
        if(k==0. && col==0) break;
        dist = 0.01;
        rayStartPosition = rayPosition + normalVector;
        rayPosition = rayStartPosition;
        rayDirection = reflect(rayDirection, normalVector);
    } 
    returnColor.rgb-=vec3(fog);
    return returnColor;
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