precision lowp float;
uniform sampler2D mixerTex;
uniform float iGlobalTime;
uniform vec2 iResolution;

float rand(vec2 co) {
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float rand(float c){
    return rand(vec2(c));
}
void main(){
    vec2 p = gl_FragCoord.xy / iResolution.xy;
    p.x+=(rand(p.y*100.+iGlobalTime)-.5)/155.;
    p*=1.-iGlobalTime/77.;
    float px=p.x*(iResolution.x/iResolution.y);
    px-=0.25*(iResolution.x/iResolution.y);
    px=max(min(px,1.),0.);
    p.x=px;
    gl_FragColor =  (1.+sin(iGlobalTime*4.)/4.)*texture2D(mixerTex, p*vec2(1.,-1.))/(iGlobalTime*iGlobalTime+1.);
}