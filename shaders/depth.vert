#version 460 core
// Vertex attributes
layout (location = 0) in vec3 aPos;
layout (location = 3) in vec2 aTexCoords;
layout (location = 4) in uvec4 aJoint;
layout (location = 5) in vec4 aWeight;

// Storage buffers
layout(binding = 0, std430) readonly buffer modelMatricesSsbo {
  mat4 modelMatrices[];
};

layout(binding = 1, std430) readonly buffer transformationMaticesSsbo {
  // Indexed by instance ID * number of nodes + node ID.
  mat4 transformationMatrices[];
};

layout(binding = 2, std430) readonly buffer skinNumJointsSsbo {
  // Indexed by instance ID * number of joints + skin ID * skin offset + joint
  // ID.
  int skinNumJoints[];
};

layout(binding = 3, std430) readonly buffer jointMatricesSsbo {
  // Indexed by joint ID.
  mat4 jointMatrices[];
};

// Uniforms
uniform int nodeId;
uniform int numNodes; // Number of mesh nodes in the model.
uniform int skinId;
uniform int numJoints; // Number of joints in the model.
uniform mat4 viewM;
uniform mat4 projectionM;

// Outputs
out vec2 TexCoords;

void main()
{
  mat3 normalM;
  mat4 modelM = modelMatrices[gl_InstanceID];
  int transformationOffset = gl_InstanceID * numNodes + nodeId;
  mat4 transformationM = transformationMatrices[transformationOffset];

  // Calculate vertex world position
  vec4 WorldPos;

  if (skinId >= 0) {
    int jointOffset = gl_InstanceID * numJoints
                        + skinId * skinNumJoints[skinId];
    // Calculate the skin matrix
    mat4 skinM =
      aWeight.x * jointMatrices[jointOffset + aJoint.x] +
      aWeight.y * jointMatrices[jointOffset + aJoint.y] +
      aWeight.z * jointMatrices[jointOffset + aJoint.z] +
      aWeight.w * jointMatrices[jointOffset + aJoint.w];

    // Our skinned meshes/inverse bind matrices are already pre-multiplied
    // by the bind shape matrix of their joint so we dont't need to do that
    // again.
    normalM = mat3(transpose(inverse(modelM * skinM)));
    WorldPos = modelM * skinM * vec4(aPos, 1.0);
  } else {
    normalM = mat3(transpose(inverse(modelM * transformationM)));
    WorldPos = modelM * transformationM * vec4(aPos, 1.0);
  }

  TexCoords = aTexCoords;
  gl_Position = projectionM * viewM * WorldPos;
}
