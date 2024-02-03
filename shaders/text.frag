#version 460 core
in vec2 TexCoord;

out vec4 FragColor;

uniform sampler2D msdf;
/*
uniform vec4 foregroundColor;
*/

vec4 foregroundColor = vec4(1.0, 1.0, 1.0, 1.0);
vec4 backgroundColor = vec4(0.0, 0.0, 0.0, 0.0);

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}

float screenPxRange() {
    return 32.0;
}

void main() {
    vec3 msd = texture(msdf, TexCoord).rgb;
    float sd = median(msd.r, msd.g, msd.b);
    float screenPxDistance = screenPxRange()*(sd - 0.5);
    float opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0);
    FragColor = mix(backgroundColor, foregroundColor, opacity);
}
