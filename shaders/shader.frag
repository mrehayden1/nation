#version 330 core
in vec3 aPos;
out vec4 FragColor;

void main()
{
    FragColor = vec4(aPos.x + 0.5f, 0.0f, 0.0f, 1.0f);
} 
