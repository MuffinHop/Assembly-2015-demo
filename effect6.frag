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
    vec3 tex=texture2D(mixerTex,p.xz/16.+vec2(.25)).rgb;
    vec3 tex0=texture2D(mixerTex,p.xz/111.+vec2(.25)).rgb;
    p.y+=pow(sin(length(p.xz)-iGlobalTime/2.+atan(p.x,p.z)*3.)*2.+length(p.xz),1.4)/22.;
    finalDistance=sdBox(p, vec3(10000.,2.,10000.));
    vec3 rp=p;
    p.x=mod(p.x,5.)-2.5;
    p.z=mod(p.z,5.)-2.5;
    p.y+=sin(length(rp.xz)*4.+sin(p.x/4.2))/8.;
    finalDistance=smin(finalDistance,sdSphere(p-vec3(0.,sin((floor(rp.z/5.)+floor(rp.x/5.))*10.)/2.,0.), 1.6),0.8);
    finalDistance+=tex.r/2.+tex0.r*2.;
    color.rgb=tex*vec3(0.5,0.3,0.4)*3.;
    rp*=rotationMatrix(vec3(1.6,0.2,0.1),p.y/33.);
    vec3 rrp=rp;

    float c=sdCapsule( rp, vec3(0.,0.,0.), vec3(0.,10.8,0.), 1.8+p.y*p.y/99. );
    vec3 rrrp=rrp;
    rrrp.x=mod(rrrp.x,1.)-0.5;
    rrrp.y=mod(rrrp.y,0.5)-0.25;
    rrrp.z=mod(rrrp.z,1.)-0.5;
    c=smax(c,sdSphere(rrrp,0.9),3.1);
    rp.y+=sin(length(rp.xz)/2.)+length(rp.xz)/8.;
    for(float i=0.; i<6.; i++)
        c=smin( c, 
                sdBox( 
                rp * 
                    rotationMatrix(
                        vec3(   0.,
                                1.,
                                sin(i*15.+length(rp.xz)/12.)/14.),
                        i*3.14159/3.) - 
                vec3(
                    0.,
                    12.+sin(i)/5.,
                    0.), 
                vec3(
                    18.-sin(length(p.xz)/5.4)*7.,
                    .4-length(p.xz)/12.,
                    4.-sin(length(p.xz+3.14159/3.)/11.4)*3.) ), 
                2.8);
    vec3 tex2=texture2D(mixerTex,p.xz/12.).rgb;
    c+=(tex.r+tex2.r)/6.;
    /*
    for(float i=0.; i<3.; i++)
        c=min(c, sdBox(rrp+vec3(rand(i+77.)-.5,sin(i*2.+iGlobalTime)/7.,rand(i+111.)-.5)*15., vec3(2.4+2.*rand(i+2.),5.,2.4+2.*rand(i))));*/
    if(c<finalDistance) {
        finalDistance=c;
        color.rgb=-tex2+vec3(1.8,1.2,1.2+(min(pow(length(rrp.xz),4.)/66.,1.9)));
        reflectiveValue = 1.2;
    }
    color.a=0.3;
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
    for( int i=0; i<33; i++ )
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
    for(float k=0.; k<2.; k++) {
        for(float i=0.; i<33.; i++) {
            vec4 color;
            float stepable = calculateDistance(rayPosition, color, reflectiveValue);
            dist += stepable;
            rayPosition = rayStartPosition + dist * rayDirection;
            if(length(rayPosition)>22.) break;
            if( abs(stepable) <= epsilon){
                collisions++;
                vec3 C;
                float dummy = 0.0;

                vec3 tex=texture2D(mixerTex,vec2(atan(rayPosition.x,rayPosition.y),rayPosition.z/8.)).rgb;
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
        }
        dist = 0.01;
        rayStartPosition = rayPosition + normalVector;
        rayPosition = rayStartPosition;
        rayDirection = reflect(rayDirection, normalVector);
    } 
    for(float i=0.; i<10.; i++) {
    vec2 postex=vec2(atan(rayDirection.y,rayDirection.z)/3.-.4,-rayDirection.x/3.-.5+(rand(i)-iGlobalTime)/100.);
        returnColor.rgb+=0.1*abs(rayDirection.y*2.+3.14159/2.)*pow(texture2D(mixerTex3,postex).rgb,vec3(2.))/(collisions*collisions*collisions)*(1.-abs(rayDirection.x));
    }
    for(float i=0.; i<10.; i++) {
    vec2 postex=vec2(atan(rayDirection.y,rayDirection.z)/3.-.4,-rayDirection.x/3.-.44+(rand(i)-iGlobalTime*1.3)/100.);
        returnColor.rgb+=0.1*abs(rayDirection.y*2.+3.14159/2.)*pow(texture2D(mixerTex3,postex).rgb,vec3(2.))/(collisions*collisions*collisions)*(1.-abs(rayDirection.x));
    }
    return returnColor;
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
    
    vec2 uv = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    float aspect = iResolution.x / iResolution.y;
        gl_FragColor = piip();
}