#version 460 core

in vec2 TexCoords;

layout (binding = 0) uniform sampler2D albedoTexture;

uniform int alphaMode;
uniform float alphaCutoff;

const int ALPHA_MODE_MASK = 1;

void main() {
  float alpha = texture(albedoTexture, TexCoords).a;

  // FIXME Alpha cutoff
  if (alphaMode == ALPHA_MODE_MASK && alpha < alphaCutoff)
    discard;
}
