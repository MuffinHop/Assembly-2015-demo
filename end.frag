precision lowp float;

uniform sampler2D mixerTex;
uniform sampler2D mixerTex2;
uniform sampler2D mixerTex3;
uniform vec2 iResolution;
uniform float iGlobalTime;
float rand(vec2 co) {
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float rand(float c){
    return rand(vec2(c));
}
void main(){
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    vec2 p = 2.0 * gl_FragCoord.xy / iResolution.xy - 1.0;
    gl_FragColor = (vec4(vec3(max(1.-iGlobalTime,0.)),1.)+3.*texture2D(mixerTex, uv*vec2(1.,-1.))+vec4((1./(1.+length(p)/2.))*3.*vec3(rand(uv*iGlobalTime)-.5)/(iGlobalTime+1.),1.))/(1.+iGlobalTime*iGlobalTime/7.)+vec4(texture2D(mixerTex2, uv*vec2(1.,-1.+sin(iGlobalTime/88.)/12.)).r,texture2D(mixerTex2, uv*vec2(1.,-1.+sin(iGlobalTime/44.)/12.)).g,texture2D(mixerTex2, uv*vec2(1.,-1.+sin(iGlobalTime/22.)/55.)).b,1.0)+texture2D(mixerTex3, uv+vec2(iGlobalTime/44.,0.))/16.+texture2D(mixerTex3, uv+vec2(iGlobalTime/38.+.2,0.))/17.;
}