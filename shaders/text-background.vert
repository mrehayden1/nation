#version 460 core
layout (location = 0) in vec2 aPos;

uniform mat4 projectionM;

void main()
{
    gl_Position = projectionM * vec4(aPos, 0.0, 1.0);
}
