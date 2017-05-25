
float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}
float sdCapsule( vec2 p, vec2 a, vec2 b, float r ){
    vec2 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return -min(max(100.0*(length( pa - ba*h ) - r),-1.),0.);
}



mat2 rotate(float Angle){
    mat2 rotation = mat2(
        vec2( cos(Angle),  sin(Angle)),
        vec2(-sin(Angle),  cos(Angle))
    );
	return rotation;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	float vasenKulmakarva=floor(mod(iGlobalTime*0.8,2.0))*0.1;
	float oikeaKulmakarva=floor(mod(iGlobalTime*0.3,2.0))*0.1;
	float vasenSilma=min(max(0.24*sin(iGlobalTime),0.006),0.06);
	float oikeaSilma=min(max(0.24*sin(iGlobalTime),0.006),0.06);
	float suu=iGlobalTime*10.0;
	vec4 tulos;
    
	vec2 uv = fragCoord.xy / iResolution.xy;
	float aspectCorrection = (iResolution.x/iResolution.y);
	vec2 coordinate_entered = 2.0 * uv - 1.0;
    
	vec2 coord = vec2(aspectCorrection,1.0) *coordinate_entered;
    
	tulos=vec4(vec3(0.0/255.0, 23.0/255.0, 12.0/255.0),1.0);
    coord *= rotate(iGlobalTime+length(coord)/2.*sin(iGlobalTime));
    coord *= 1.0 + sin(length(coord)+iGlobalTime*3.) * 0.2;
    coord.y += sin(coord.x*3.+iGlobalTime)/32.;
    coord.x += 0.6*sin(iGlobalTime/8.);
    for(float i=0.0; i<31.0; i++) {
    	tulos.rgb+=sdCapsule(coord,vec2( 0.0, 0.0),vec2( cos(i), sin(i))*12.0, 0.02+sin(i*33.-iGlobalTime*3.+length(coord)*10.)/11.0);
    
    	tulos.rgb-=sdCapsule(coord,vec2( 0.0, 0.0),vec2( cos(i), sin(i))*12.0, 0.0016+sin(i*33.-iGlobalTime*3.+length(coord)*10.)/11.0);
    }
    
    for(float i=0.0; i<16.0; i++) {
    	coord.y += cos(i+iGlobalTime)/32. + sin(sin(coord.x+i*sin(iGlobalTime)/32.)*3.+coord.x*1.3+iGlobalTime+i+sin(i+iGlobalTime)/(8.+sin(iGlobalTime)*2.))/3.;
    	tulos.rgb += sdCapsule(coord,vec2( 888.0, 0.0),vec2(-888.0,0.0),0.005+sin(iGlobalTime+i)/666.0);
    }
	tulos.xyz-=floor(mod(fragCoord.y,32.0)/32.0 + 1./32.0) / 12.0;
	tulos.xyz-=floor(mod(fragCoord.x,32.0)/32.0 + 1./32.0) / 12.0;
    tulos.rgb-=rand(coord*iGlobalTime)/12.0;
	tulos.xyz=tulos.xyz/1.3 * mod(fragCoord.y,2.0);
    tulos.rgb*=vec3(0.0,1.0,0.4);
	fragColor = tulos;
}