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
float hexacon(vec2 coord, vec2 pos, float size, float thickness){
    coord -= pos;
    float COLOR=0.;
    COLOR += 2.*sdCapsule(coord, vec2(0.33,-0.5)*size, vec2(-0.33,-0.5)*size, thickness);
    COLOR += 2.*sdCapsule(coord, vec2(0.33,-0.5)*size, vec2(0.66,0.0)*size, thickness);
    COLOR += 2.*sdCapsule(coord, vec2(0.33,0.5)*size, vec2(0.66,0.0)*size, thickness);
    COLOR += 2.*sdCapsule(coord, vec2(0.33,0.5)*size, vec2(-0.33,0.5)*size, thickness);
    COLOR += 2.*sdCapsule(coord, vec2(-0.33,0.5)*size, vec2(-0.66,0.0)*size, thickness);
    COLOR += 2.*sdCapsule(coord, vec2(-0.33,-0.5)*size, vec2(-0.66,0.0)*size, thickness);
    return COLOR;
}
mat2 rotate(float Angle)
{
    mat2 rotation = mat2(
        vec2( cos(Angle),  sin(Angle)),
        vec2(-sin(Angle),  cos(Angle))
    );
	return rotation;
}
float outlinetexture(){
	vec2 p = gl_FragCoord.xy / iResolution.xy;
    return floor(length(texture2D(iChannel2, p).rgb)*1.04);
}
void mainImage( out vec4 fragColor, in vec2 fragCoord ){
	vec2 p = fragCoord.xy / iResolution.xy;
	float aspectCorrection = (iResolution.x/iResolution.y);
	vec2 coordinate_entered = 2.0 * p - 1.0;
	vec2 coord = vec2(aspectCorrection,1.0) *coordinate_entered;
    vec2 s = coord*(1.0+sin(iGlobalTime+coord.x*1.5+coord.y*0.8)*0.1);
	float vignette = 1.0 / max(0.25 + 0.5*dot(coord,coord),1.);
    coord *= 6.;
    coord.x = mod(coord.x+floor(coord.y*1.)*.5,1.)-.5;
    coord.y = mod(coord.y,1.)-.25;
    
    float modulotime = mod(iGlobalTime,10.);
	s *= rotate(iGlobalTime*0.4+3.);
    s /= 0.3+0.64*(cos(s.x)+cos(s.y));
    s.x+=cos(iGlobalTime*0.1)*13.;
    s.y+=(iGlobalTime*4.5);
	vec3 COLOR =(vec3(0.9,0.4,0.5)
        		+vec3(floor(mod(s.y,1.)*3.+.015))
        		*vec3(0.2,0.5,0.2) );
    COLOR += .1*sdCapsule(coord, vec2(-0.15,-0.15), vec2(0.15,0.15), 0.05);
    COLOR += .1*sdCapsule(coord, vec2(0.15,-0.15), vec2(-0.15,0.15), 0.05);
    coord = vec2(mod(s.x,.999)-.333,mod(s.y,.75)-0.21);
   	COLOR -= hexacon(coord, vec2(0.0), .3-0.1*sin(iGlobalTime*5.+s.x+s.x), 0.02);
   	COLOR -= hexacon(coord, vec2(0.5,0.333), .3-0.1*sin(iGlobalTime*5.+s.x), 0.02);
   	COLOR -= hexacon(coord, vec2(-0.5,0.333), .3-0.1*sin(iGlobalTime*5.+s.x), 0.02);
    
	fragColor = vec4( (texture2D(iChannel0,s).rgb*.04+.7*(COLOR))*vignette*vec3(1.8,1.,1.3)
         				,1.0);
        
} 