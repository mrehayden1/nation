#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out vec3 WorldPos;
out vec3 Normal;
out vec2 TexCoords;
out vec4 LightClipPos; // For shadow calculations

uniform mat4 modelM;
uniform mat4 viewM;
uniform mat4 projectionM;
uniform mat4 normalM; // Can't pass 3x3 matrices with the OpenGL bindings
uniform mat4 lightViewM;
uniform mat4 lightProjectionM;

void main()
{
  WorldPos = vec3(modelM * vec4(aPos, 1.0));
  Normal = normalize(mat3(normalM) * aNormal);
  TexCoords = aTexCoords;
  LightClipPos = lightProjectionM * lightViewM * vec4(WorldPos, 1.0);
  gl_Position = projectionM * viewM * vec4(WorldPos, 1.0);
}
