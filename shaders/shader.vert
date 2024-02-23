#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec4 aTangent;
layout (location = 3) in vec2 aTexCoords;

out vec3 WorldPos;
out vec3 Normal;
out mat3 TBN;
out vec2 TexCoords;
out vec4 LightClipPos; // For shadow calculations

uniform mat4 modelM;
uniform mat4 viewM;
uniform mat4 projectionM;
uniform mat4 normalM;
uniform mat4 lightViewM;
uniform mat4 lightProjectionM;

void main()
{
  WorldPos = vec3(modelM * vec4(aPos, 1.0));

  mat3 normalM3 = mat3(normalM); // Can't pass mat3 with our OpenGL bindings
  Normal = normalize(normalM3 * aNormal);
  vec3 Tangent = normalize(normalM3 * vec3(aTangent));
  vec3 Bitangent = cross(Normal, Tangent) * aTangent.w;
  TBN = mat3(Tangent, Bitangent, Normal);

  TexCoords = aTexCoords;
  LightClipPos = lightProjectionM * lightViewM * vec4(WorldPos, 1.0);
  gl_Position = projectionM * viewM * vec4(WorldPos, 1.0);
}
