#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;

out vec4 fragColor;

uniform mat4 modelM;
uniform mat4 viewM;
uniform mat4 projectionM;

void main()
{
  gl_Position = projectionM * viewM * modelM * vec4(aPos, 1.0);
  fragColor = aColor;
}
