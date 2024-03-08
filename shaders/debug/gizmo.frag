#version 460 core
in vec4 fragColor;
in vec2 TexCoords;

layout (binding = 0) uniform sampler2D baseColorTexture;

uniform bool hasBaseColorTexture;

uniform vec4 baseColorFactor;

out vec4 FragColor;

void main()
{
  if (hasBaseColorTexture) {
    FragColor = texture(baseColorTexture, TexCoords).rgba * baseColorFactor;
  } else {
    FragColor = baseColorFactor;
  }
}
