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
uniform float iLocalTime;

mat2 rotate(float Angle)
{
    mat2 rotation = mat2(
        vec2( cos(Angle),  sin(Angle)),
        vec2(-sin(Angle),  cos(Angle))
    );
    return rotation;
}
struct polygon{
	vec2 A, B, C;
};
float sign(vec2 p1, vec2 p2, vec2 p3){
  return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
}
int PointInTriangle(vec2 pt, vec2 v1, vec2 v2, vec2 v3){
	int b1, b2, b3;

	if(sign(pt, v1, v2) < 0.0) b1=1;
	if(sign(pt, v2, v3) < 0.0) b2=1;
	if(sign(pt, v3, v1) < 0.0) b3=1;
	if((b1 == b2) && (b2 == b3))
		return 1;
	return 0;
}

int PointInTriangle(vec2 pt, polygon X){
	int b1, b2, b3;

	if(sign(pt, X.A, X.B) < 0.0) b1=1;
	if(sign(pt, X.B, X.C) < 0.0) b2=1;
	if(sign(pt, X.C, X.A) < 0.0) b3=1;
	if((b1 == b2) && (b2 == b3))
		return 1;
	return 0;
}

float box(vec2 coord, vec2 pos, vec2 size){
	if((coord.x<(pos.x+size.x)) &&
	   (coord.x>(pos.x-size.x)) &&
	   (coord.y<(pos.y+size.y)) && 
	   (coord.y>(pos.y-size.y)) ) 
		return 1.0;
	return 0.0;
}
float sun(vec2 coord, vec2 pos, float size){
	if(length(coord-pos)<size)
		return 1.0;
	return 0.0;
}

float rand(vec2 co) {
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float rand(float c){
    return rand(vec2(c));
}
float sdCapsule( vec2 p, vec2 a, vec2 b, float r ){
    vec2 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return min(floor(length( pa - ba*h ) - r),0.);
}
float triangle( vec2 coord, vec2 pos, float angle, float thick, float size){
    vec2 original_coord = coord;
    coord += pos;
    coord *= rotate(angle);
    float collision = 0.0;
    collision += sdCapsule( coord, vec2( 0.0, 0.333)*size, vec2( 0.3,-0.2)*size, thick );
    collision += sdCapsule( coord, vec2( 0.0, 0.333)*size, vec2(-0.3,-0.2)*size, thick );
    collision += sdCapsule( coord, vec2( 0.3,-0.2)*size, vec2(-0.3,-0.2)*size, thick );
    return -min(max(-collision,0.0),1.0);
}

float circle(vec2 coord, float size, float start, float end){
    float angle = atan(coord.x, coord.y);
    if(angle>start && angle<end) return 0.0;
    return min(floor(distance(coord,vec2(0.))-size),0.);
}
vec4 mainImage(){
    vec2 coord = 1.8 * vec2((iResolution.x/iResolution.y),1.0) *(gl_FragCoord.xy / iResolution.xy - 0.5) / 2.;
    coord*=1.0+rand(coord+max(iGlobalTime-5.,0.)/2.)/(pow(max(iGlobalTime-5.,0.)/2.,7.0)*3.0)-length(coord)*10.0/(pow(max(iGlobalTime-5.,0.)*1.1/2.,24.0));
    vec2 p=coord;
    coord*=0.7;
    float vignette = 1.0 / max(0.3 + 1.7*dot(coord,coord),1.);
    vec3 COLOR =vec3(0.7,0.77,0.9);
    for(float ttt=0.; ttt<0.4; ttt+=0.1){
    float time=ttt+iGlobalTime*(0.5+0.114*sin(iGlobalTime*0.1));
    coord*=sin(time*0.001)*0.1+0.6;
    coord*=rotate(time*0.1);
    coord*=1.0+0.4*sin(iGlobalTime*0.2+length(coord)*0.1);
    coord *= sin(cos(length(coord)*1.4-time*0.42)*4.+time*.1)*1.6;
        
    coord += 0.01*(rand(vec2(floor(coord.x*10.),floor(coord.y*44.)+ttt))-0.5);
        
        
        
    coord.x +=0.06*(rand(vec2(floor(coord.y*12.+time)))-0.5);
    coord.y +=0.01*(rand(vec2(floor(coord.x*777.+time)))-0.5);
    for(float i=0.; i<33.; i++)
        COLOR -= (vec3(sin(i+time*.78),cos(i+time*.64+length(coord)*4.),sin(time))*0.05+0.3)*circle(coord,0.01*i+0.4*sin(time+i),sin(i*4.3+time*0.1)*3.141,sin(i*0.3+time*0.1)*3.141) - 0.3*circle(coord,-0.02+0.01*i+0.4*sin(time+i),sin(i*4.3+time*0.1)*3.141,sin(i*0.3+time*0.1)*3.141);
    if(mod(p.y*2e2+p.x*2e2,3.)<1.1)
        COLOR -= vec3(0.05);
    COLOR += 0.2*(cos(time)*0.16 + 1.1 - min(time, 1.0));
    COLOR+=0.1*(1.0+mod(time*5.0,1.0)*0.1)-
        vec3(
        rand(p+.21*vec2(time))*rand(vec2(p.x,time*.3)),
        rand(vec2(coord.y,0.)+p+.31*vec2(time))*rand(vec2(p.x*0.01,time*2.1)),
        rand(p+.41*vec2(time))*rand(vec2(p.x*0.1,time))
        )*.4;
    COLOR+=texture2D(mixerTex,coord).rgb;
    }
    COLOR/=2.6;
    return vec4( COLOR*vignette
                        ,1.0);
}

void main(  ){
	vec4 tulos;
	vec4 lopullinentulos=vec4(1.0);
	vec2 uv = gl_FragCoord.xy / iResolution.xy;
	float aspectCorrection = (iResolution.x/iResolution.y);
	vec2 coordinate_entered = 2.0 * uv - 1.0;
	vec2 coord = vec2(aspectCorrection,1.0) *coordinate_entered * 2.;
    vec2 nc=coord;
    vec2 ori_coord=coord;
	for(float rgbare=0.0; rgbare<2.0; rgbare++){
	coord = vec2(aspectCorrection,1.0) *coordinate_entered * (1.+0.2*cos(iGlobalTime*0.005));
	coord.x*=1.0+rgbare*0.02;
	coord*=1.0+rand(coord+max(iGlobalTime-5.,0.)/2.)/(pow(max(iGlobalTime-5.,0.)/2.,7.0)*3.0)-length(coord)*10.0/(pow(max(iGlobalTime-5.,0.)*1.1/2.,24.0));
	coord*=1.0+0.1*sin(1.01*0.1);
	tulos=vec4(vec3(0.0/255.0, 0.0/255.0, 0.0/255.0),1.0);
    
    float beat = ceil(iGlobalTime/4.);
    coord*=1.0+0.3*sin(iGlobalTime*1.)/(1.+33.*mod(iGlobalTime,1.));
	
 
        
    for(float i=0.; i<32.; i++)
	if(mod(coord.x+coord.y+1.01*0.1+iGlobalTime/4.,0.2)<0.1){
        vec2 arrowcoord=coord*rotate(iGlobalTime*3.1415/8.+i*3.14159/4.)-vec2(0.,1.+rand(floor(iGlobalTime/4.+i))+mod(iGlobalTime,4.)/4.);
        arrowcoord*=mod((iGlobalTime+i)/4.,4.)+1.;
		if(box(arrowcoord,vec2(0.),vec2(4.,0.4))==1.)
		   	tulos.xyz-=vec3(0.4,0.5,0.3)*0.1;
        if(PointInTriangle(
        							arrowcoord, 
        							vec2(0.,.5),
        							vec2(-.5,0.),
        							vec2(.5,0.) ) == 1)
		   	tulos.xyz+=vec3(1.1);
        if(box(arrowcoord,vec2(0.,-.25),vec2(.25,0.25))==1. )
		   	tulos.xyz=vec3(1.);
        
	}
        
        
    float collision = 0.0;
    if(collision<0.0)
        tulos.xyz += vec3(0.1);
	
	tulos.xyz=tulos.xyz-vec3(min(max(-0.57+length(ori_coord)*0.32,0.0),1.0))+vec3(0.015+0.03*rand(vec2(ori_coord.x+ori_coord.y,1.01*ori_coord.y*ori_coord.x)));
	
	if(rgbare==0.0)
		lopullinentulos.r=tulos.r;
	if(rgbare==1.0)
		lopullinentulos.gb=tulos.gb;
	}
	lopullinentulos.xyz=lopullinentulos.xyz*(1.2-0.4*mod(gl_FragCoord.y,2.0));
    lopullinentulos.rgb = vec3(length(lopullinentulos.rgb));
	lopullinentulos.xyz+=floor(mod(nc.y*1000.0,32.0)/32.0 + 2./32.0) / 12.0;
	lopullinentulos.xyz+=floor(mod(nc.x*1000.0,32.0)/32.0 + 2./32.0) / 12.0;
    lopullinentulos.rgb-=rand(coord*iGlobalTime)/12.0;
	lopullinentulos.xyz=lopullinentulos.xyz/1.3 * mod(gl_FragCoord.y,2.0);
	gl_FragColor = vec4(texture2D(mixerTex,uv*vec2(1.,-0.6)-vec2(0.,.2)).rgb*(-lopullinentulos.rgb*2.+mainImage().rgb),1.);
}