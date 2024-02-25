#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 3) in vec2 aTexCoords;

out vec2 TexCoords;

uniform mat4 projectionM;
uniform mat4 viewM;
uniform mat4 modelM;

void main()
{
  TexCoords = aTexCoords;
  gl_Position = projectionM * viewM * modelM * vec4(aPos, 1.0);
}
