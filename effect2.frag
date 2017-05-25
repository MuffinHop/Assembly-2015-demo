precision lowp float;
uniform sampler2D mixerTex;
uniform sampler2D mixerTex2;
uniform sampler2D mixerTex3;
uniform float iGlobalTime;
uniform vec2 iResolution;
uniform vec3 light;
uniform float lightPower;
uniform float shadowItr;
uniform float marchItr;

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
    p*=rotationMatrix(vec3(0.,0.,1.),sin(iGlobalTime+p.z/64.));
    vec3 rp=p;
    rp*=rotationMatrix(vec3(1.,0.,0.),sin(iGlobalTime)/8.);
    rp*=rotationMatrix(vec3(0.,0.,1.),sin(iGlobalTime*1.4)/5.);
    vec3 crp=rp;
    rp.x+=sin(iGlobalTime)*16.;
    float dist0=ship(rp);
    p.z+=iGlobalTime*100.;
        vec3 tex=texture2D(mixerTex,rp.xz/16.).rgb;
        vec3 tex2=texture2D(mixerTex2,rp.zx/13.).rgb;
        vec3 tex3=texture2D(mixerTex,p.zx/44.).rgb;
    //p.z+=iGlobalTime*64.;
    vec3 p2=p;
    //p.x+=sin(p.z/24.)*12.+sin(p.z/44.)*12.;
    finalDistance=min(finalDistance, sdBox( p-vec3(0.,-8.,0.), vec3(54.,2.,111111.)));
    finalDistance=max(finalDistance, -sdBox( p-vec3(0.,-6.,0.), vec3(48.,2.,111111.)));
    
    finalDistance-=tex3.r/12.;
    color.rgb=tex3;
    reflectiveValue=7.;
    if(dist0<finalDistance){
        finalDistance=dist0+tex2.r/6.+tex.r/64.;
        color.rgb= tex2;
        reflectiveValue=3.;
        color.a=0.01;
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
    return clamp(res,0.1,9.0);
}
vec4 tracer(vec3 rayStartPosition, vec3 rayDirection) {
    const float epsilon = 1./32.;
    
    vec3 rayPosition = rayStartPosition;

    vec3 normalVector;
    float dist = 0.0;
    vec4 returnColor = vec4(0.0);
    vec4 finalColor = vec4(0.0);
    vec3 lightSource = vec3(sin(iGlobalTime*iGlobalTime/10.)*100.,cos(iGlobalTime*iGlobalTime/10.)*100.,100.);
    vec3 lightColor = vec3(1.1,1.0,1.0) * 2.;
    float reflectiveValue = 1.0;
    float reflectionNow = 100.0;
    float finalLight = 1.0;
    vec4 endlighted = vec4(0.);;
    float possibleFogLight = 1.0;
    float firstBunchDist=100000.;
    float collisions=1.;
    for(float k=0.; k<2.; k++) {
        for(float i=0.; i<48.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;

            if(length(rayPosition.xy)>60.) break;
            if(abs(rayPosition.z)>160.) break;
            if( abs(stepable) <= epsilon){
                collisions++;
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
                
                
                returnColor += vec4( vec3(finalLight*max(1.5*directLight*lightColor,0.05)) / (k*2.8/reflectionNow + 1.0), 1.) * finalColor;
                reflectionNow = min(reflectionNow,reflectiveValue);
  
                    
                firstBunchDist = i;
                break;
                
            }
                if(k==0.) {
                    vec3 lightDir = (lightSource-rayStartPosition);lightDir = normalize(lightDir);
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
    vec3 cameraPosition = vec3( 0., 32., -33.);
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    
    float aspect = iResolution.x / iResolution.y;
    vec3 direction = normalize(vec3(.5 * uv * vec2(aspect, 1.0), 1. ));
    direction *= rotationMatrix(vec3(1.,0.,0.),0.4);
    cameraPosition*=rotationMatrix(vec3(0.,1.,0.),sin(iGlobalTime/16.));
    direction*=rotationMatrix(vec3(0.,1.,0.),sin(iGlobalTime/16.));
    return tracer(cameraPosition, direction);
}

void main(){
    
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
        gl_FragColor = piip();
}