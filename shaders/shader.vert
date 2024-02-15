#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;

out vec4 LightClipPosition;
out vec3 Normal; // In view space
out vec3 WorldPosition;

uniform mat4 lightViewM;
uniform mat4 lightProjectionM;
uniform mat4 modelM;
uniform mat4 viewM;
uniform mat4 projectionM;

void main()
{
  // TODO pass the normal matrix in as a uniform
  Normal = normalize(mat3(transpose(inverse(viewM * modelM))) * aNormal);
  LightClipPosition = lightProjectionM * lightViewM * modelM * vec4(aPos, 1.0);
  WorldPosition = vec3(viewM * modelM * vec4(aPos, 1.0));
  gl_Position = projectionM * viewM * modelM * vec4(aPos, 1.0);
}
