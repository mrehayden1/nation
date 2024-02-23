#version 460 core
in vec4 LightClipPos;
in vec3 Normal;
in mat3 TBN;
in vec3 WorldPosition;
in vec2 TexCoords;

out vec4 FragColor;

layout (binding = 0) uniform sampler2D lightDepthMap;
layout (binding = 1) uniform sampler2D albedoTexture;
layout (binding = 2) uniform sampler2D normalMap;

uniform float ambientIntensity;
uniform vec3 lightDirection;
uniform mat4 viewM;

float ShadowCalculation(vec4 clipPos)
{
  // perform perspective divide
  vec3 ndcCoords = clipPos.xyz / clipPos.w;
  // transform to [0,1] range
  vec3 mapCoords = ndcCoords * 0.5 + 0.5;
  float shadow = 0.0;
  // If the depth of our point is greater than the depth the map has sampled
  // then don't render any shadow.
  if (mapCoords.z < 1.0) {
    // get depth of current fragment from light's perspective
    float currentDepth = mapCoords.z;
    // calculate shadow using percentage close filtering
    vec2 texelSize = 1.0 / textureSize(lightDepthMap, 0);
    float bias = max(0.005 * (1.0 - dot(Normal, lightDirection)), 0.005);
    int pcfWidth = 3;
    int pcfXYMin = -(pcfWidth - 1) / 2;
    int pcfXYMax = pcfWidth / 2;
    for(int x = pcfXYMin; x <= pcfXYMax; ++x)
    {
      for(int y = pcfXYMin; y <= pcfXYMax; ++y)
      {
        float pcfDepth = texture(
          lightDepthMap, mapCoords.xy + vec2(x, y) * texelSize
        ).r;
        shadow += currentDepth - bias > pcfDepth ? 1.0 : 0.0;
      }
    }
    shadow /= pow(pcfWidth, 2);
  }
  return shadow;
}

void main()
{
  vec3 lightColour = vec3(1.0f, 1.0f, 1.0f);

  // Normal mapping
  vec3 normal = texture(normalMap, TexCoords).rgb;
  normal = normal * 2.0 - 1.0;
  normal = normalize(TBN * normal);
  // Ambient light
  vec3 ambient = ambientIntensity * lightColour;
  // Diffuse light
  vec3 diffuse = max(dot(normal, lightDirection), 0.0f) * lightColour;
  // Shadow
  float shadow = ShadowCalculation(LightClipPos);

  vec3 lighting = (ambient + (1.0 - shadow) * diffuse) * texture(albedoTexture, TexCoords).rgb;

  FragColor = vec4(0.0, 0.0, 0.0, 1.0);

  // Gamma correction
  float gamma = 2.2;
  FragColor.rgb = pow(lighting.rgb, vec3(1.0/gamma));
}
