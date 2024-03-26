#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec4 aTangent;
layout (location = 3) in vec2 aTexCoords;
layout (location = 4) in uvec4 aJoint;
layout (location = 5) in vec4 aWeight;

out vec4 WorldPos;
out vec3 Normal;
out mat3 TBN;
out vec2 TexCoords;
out vec4 LightClipPos; // For shadow calculations

uniform mat4 modelM;
uniform mat4 bindM;
uniform mat4 viewM;
uniform mat4 projectionM;
uniform mat4 lightViewM;
uniform mat4 lightProjectionM;

uniform bool skinned;
uniform mat4 jointM[128];

void main()
{
  mat3 normalM;

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

  Normal = normalize(normalM * aNormal);
  vec3 Tangent = normalize(normalM * aTangent.xyz);
  vec3 Bitangent = cross(Normal, Tangent) * aTangent.w;
  TBN = mat3(Tangent, Bitangent, Normal);

  TexCoords = aTexCoords;
  LightClipPos = lightProjectionM * lightViewM * WorldPos;
  gl_Position = projectionM * viewM * WorldPos;
}
