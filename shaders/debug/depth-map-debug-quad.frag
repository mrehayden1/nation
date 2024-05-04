#version 460 core
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D depthMap;

//uniform float near_plane;
//uniform float far_plane;

// required when using a perspective projection matrix
/*
float LinearizeDepth(float depth)
{
  float z = depth * 2.0 - 1.0; // Back to NDC
  return (2.0 * near_plane * far_plane) / (far_plane + near_plane - z * (far_plane - near_plane))
}
*/

void main()
{
  //float depthValue = texture(depthMap, TexCoords).r;
  //FragColor = vec4(vec3(depthValue), 1.0); // orthographic
  //FragColor = vec4(vec3(LinearizeDepth(depthValue) / far_plane), 1.0); // perspective

  // Uniform blue (for testing)
  //FragColor = vec4(0.0f, 0.0f, 1.0f, 1.0f);

  // Regular texture
  FragColor = texture(depthMap, TexCoords);
}
