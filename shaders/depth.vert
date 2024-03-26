#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 3) in vec2 aTexCoords;
layout (location = 4) in uvec4 aJoint;
layout (location = 5) in vec4 aWeight;

out vec2 TexCoords;

uniform mat4 modelM;
uniform mat4 bindM;
uniform mat4 viewM;
uniform mat4 projectionM;

uniform bool skinned;
uniform mat4 jointM[128];

void main()
{
  mat3 normalM;
  vec4 WorldPos;

  if (skinned) {
    // Calculate the skin matrix
    mat4 skinM =
      aWeight.x * jointM[aJoint.x] +
      aWeight.y * jointM[aJoint.y] +
      aWeight.z * jointM[aJoint.z] +
      aWeight.w * jointM[aJoint.w];

    // Skinned meshes geometry or inverse bind matrices are already pre-
    // multiplied by the "bind shape matrix" of their joint so we shouldn't do
    // that again.
    normalM = mat3(transpose(inverse(modelM * skinM)));
    WorldPos = modelM * skinM * vec4(aPos, 1.0);
  } else {
    normalM = mat3(transpose(inverse(modelM * bindM)));
    WorldPos = modelM * bindM * vec4(aPos, 1.0);
  }

  TexCoords = aTexCoords;
  gl_Position = projectionM * viewM * WorldPos;
}
