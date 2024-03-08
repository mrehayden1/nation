#version 460 core

in vec2 TexCoords;

layout (binding = 0) uniform sampler2D baseColorTexture;

uniform bool hasBaseColorTexture;

uniform int alphaMode;
uniform float alphaCutoff;
uniform vec4 baseColorFactor;

const int ALPHA_MODE_MASK = 1;

void main() {
  float alpha;

  if (hasBaseColorTexture) {
    alpha = (texture(baseColorTexture, TexCoords) * baseColorFactor).a;
  } else {
    alpha = baseColorFactor.a;
  }

  // FIXME Alpha cutoff
  if (alphaMode == ALPHA_MODE_MASK && alpha < alphaCutoff)
    discard;
}
