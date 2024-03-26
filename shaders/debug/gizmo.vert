#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 3) in vec2 aTexCoords;

uniform mat4 viewM;
uniform mat4 projectionM;
uniform mat4 bindM;

out vec2 TexCoords;

void main()
{
  TexCoords = aTexCoords;
  gl_Position = projectionM * viewM * bindM * vec4(aPos, 1.0);
}
