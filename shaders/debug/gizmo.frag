#version 460 core
in vec4 fragColor;
in vec2 TexCoords;

layout (binding = 0) uniform sampler2D albedoTexture;

out vec4 FragColor;

void main()
{
  FragColor = vec4(texture(albedoTexture, TexCoords).rgb, 1.0);
}
