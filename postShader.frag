precision highp float;
uniform sampler2D scene;
uniform vec2 iResolution;
uniform float iLocalTime;
uniform float iGlobalTime;
uniform float h, s, v;
uniform float blurpower;
uniform float blurincrease;
uniform float gamma;
uniform float showvalueover;
uniform float contrast;
uniform float flippy;
uniform float flappy;
uniform float effect;
uniform float whitefadespeed;
uniform float blackfadespeed;
uniform float distrortfadespeed;
uniform float noicefadespeed;
float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}
float rand(float co){
    return rand(vec2(co));
}
vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}
vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}


vec3 render0(vec2 uv){
        if(uv.y>1. && uv.y<0.0) {
        return vec3(0.);
        }
        vec4 ret = vec4(0.0);
        vec4 tex = vec4(0.0);
        tex = texture2D(scene, uv+vec2(0.0,-0.001));
        vec3 cyan = tex.rgb*vec3(0.0,1.0,1.0);
        cyan*=1.3;
        cyan=vec3(min(cyan.r,1.0),min(cyan.g,1.0),min(cyan.b,1.0));
        tex = texture2D(scene, uv+vec2(0.0,0.001)) * distrortfadespeed;
        vec3 yellow = tex.rgb*vec3(1.0,1.0,0.0);
        yellow*=1.3;
        yellow=vec3(min(yellow.r,1.0),min(yellow.g,1.0),min(yellow.b,1.0));
        tex = texture2D(scene, uv+vec2(-0.002,0.0)) * distrortfadespeed;
        vec3 magenta = tex.rgb*vec3(1.0,0.0,1.0);
        magenta*=1.3;
        magenta=vec3(min(magenta.r,1.0),min(magenta.g,1.0),min(magenta.b,1.0));
        
        
        float depth = length(tex.rgb);
        depth*=depth;
        
        ret = vec4(cyan+yellow+magenta,1.0)/(2.0+depth/8.0);
        
        
        float aspectCorrection = (iResolution.x/iResolution.y);
        vec2 coordinate_entered = 2.0 * uv - 1.0;
        vec2 coord = vec2(aspectCorrection,1.0) *coordinate_entered;
        uv.x=floor(uv.x*200.)/200.;
        ret -= rand(uv*uv+iGlobalTime)/96.;
    ret.rgb += vec3(whitefadespeed*iLocalTime);
    ret.rgb -= vec3(blackfadespeed*iLocalTime);
    ret.rgb += vec3(noicefadespeed*iLocalTime) * (-.7+rand(vec2(floor(uv.x*80.),uv.y)*iGlobalTime)*rand(uv.y*iGlobalTime)/2.);
    return ret.rgb;
}
void main( )
{
    vec2 uv=gl_FragCoord.xy/iResolution.xy;
    if(flippy==1.0)
        uv.x=abs(-uv.x+0.5);
    if(flappy==1.0)
        uv.x=1.-uv.x;
    if(iGlobalTime<78. && iGlobalTime>77.) {
        uv.x*=(rand(uv.y+iGlobalTime)/4.)*(iGlobalTime-77.)+1.0;
    }
    uv.y *= (16./9.);
    uv.y -= .4;
        if(uv.y>.5 && uv.y<.5) {
        discard;
        }
    vec4 c=vec4(.0);
    if(uv.y<1. && uv.y>0.0) {
	    c += texture2D(scene,uv)*.1;
	    float blur=0.05+length(pow(uv-vec2(0.5),vec2(2.)))/2.;
        blur*=blurpower+iGlobalTime*blurincrease;
        for(float i=0.; i<32.0; i++) {
            c += texture2D(scene,uv+max(.8*length(uv-vec2(0.5)),1.)*blur*vec2(22.)*vec2(cos(i),sin(i))/iResolution.xy)/(32.);
        }
        vec3 bright=vec3(0.);
        for(float i=0.; i<32.0; i++) {
            float glow=texture2D(scene,vec2(sin(3.14159*i/16.),cos(3.14159*i/16.))/256.).a;
            bright+=texture2D(scene,uv+vec2(sin(3.14159*i/16.),cos(3.14159*i/16.))/256.).rgb*glow;
        }
        //c.rgb += bright;
	    c.a = 1.;
	    float vignette = (1.0) / (1.0 + 0.4*dot(uv, uv));
	    c.rgb *=vignette;
        c.rgb += vec3(-rand(uv*iGlobalTime)/128.+1./256.);
    }
    c.rgb = vec3(min(max(c.r,0.),1.),min(max(c.g,0.),1.),min(max(c.b,0.),1.));
    vec3 hsv = rgb2hsv(c.rgb);
    hsv.r *= h;
    hsv.g *= s;
    hsv.b *= v;
    c.rgb = hsv2rgb(hsv);
	vec4 endresult = vec4(c.rgb,1.);
    endresult.rgb = ((endresult.rgb - 0.5) * max(contrast, 0.)) + 0.5;
    endresult = vec4(pow(endresult.rgb,vec3(1./gamma)),endresult.a);
    endresult.r=min(max(endresult.r,0.),1.);
    endresult.g=min(max(endresult.g,0.),1.);
    endresult.b=min(max(endresult.b,0.),1.);
    if(effect==12.)
        endresult*=mod( iGlobalTime*100., 1.);
    if(showvalueover==1.) {
        if(length(endresult.rgb)>1.55) endresult.rgb=vec3(0.,1.,0.);
        if(length(endresult.rgb)<0.1) endresult.rgb=vec3(1.,0.,0.);
    }
    endresult += (-.7+rand(vec2(floor(uv.x*80.),uv.y)*iGlobalTime)*rand(uv.y*iGlobalTime)/2.)/77.;
    endresult.rgb += vec3(whitefadespeed*iLocalTime);
    endresult.rgb -= vec3(blackfadespeed*iLocalTime);
    endresult.rgb += vec3(noicefadespeed*iLocalTime) * (-.7+rand(vec2(floor(uv.x*80.),uv.y)*iGlobalTime)*rand(uv.y*iGlobalTime)/2.);
    gl_FragColor = (endresult+vec4(render0(uv)/2.,0.))*(1.-floor(mod(uv.y,2.2)));
}