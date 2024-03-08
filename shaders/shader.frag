#version 460 core
in vec4 LightClipPos;
in vec3 Normal;
in mat3 TBN;
in vec3 WorldPos;
in vec2 TexCoords;

out vec4 FragColor;

layout (binding = 0) uniform sampler2D lightDepthMap;
layout (binding = 1) uniform sampler2D baseColorTexture;
layout (binding = 2) uniform sampler2D metallicRoughnessTexture;
layout (binding = 3) uniform sampler2D normalTexture;

uniform bool hasBaseColorTexture;
uniform vec4 baseColorFactor;

uniform bool hasNormalTexture;
uniform float normalTextureScale;

uniform int alphaMode;
uniform float alphaCutoff;

uniform float ambientIntensity;
uniform vec3 camPos;
uniform bool doubleSided;
uniform vec3 lightDirection;
uniform mat4 viewM;

const int ALPHA_MODE_BLEND = 0;
const int ALPHA_MODE_MASK = 1;
const int ALPHA_MODE_OPAQUE = 2;

const float PI = 3.14159265359;

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

vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
  return F0 + (1.0 - F0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
  float a      = roughness*roughness;
  float a2     = a*a;
  float NdotH  = max(dot(N, H), 0.0);
  float NdotH2 = NdotH*NdotH;

  float num   = a2;
  float denom = (NdotH2 * (a2 - 1.0) + 1.0);
  denom = PI * denom * denom;

  return num / denom;
}

float GeometrySchlickGGX(float NdotV, float roughness)
{
  float r = (roughness + 1.0);
  float k = (r*r) / 8.0;

  float num   = NdotV;
  float denom = NdotV * (1.0 - k) + k;

  return num / denom;
}
float GeometrySmith(vec3 N, vec3 V, vec3 L, float roughness)
{
  float NdotV = max(dot(N, V), 0.0);
  float NdotL = max(dot(N, L), 0.0);
  float ggx2  = GeometrySchlickGGX(NdotV, roughness);
  float ggx1  = GeometrySchlickGGX(NdotL, roughness);

  return ggx1 * ggx2;
}

void main()
{
  vec3 lightColour = vec3(1.0f, 1.0f, 1.0f);

  vec3 baseColor;

  if (hasBaseColorTexture) {
    vec4 baseColorRgba = texture(baseColorTexture, TexCoords) * baseColorFactor;
    float alpha = baseColorRgba.a;
    baseColor = baseColorRgba.rgb;
    // Alpha cut-off
    if (alphaMode == ALPHA_MODE_MASK && alpha < alphaCutoff)
      discard;
  } else {
    baseColor = baseColorFactor.rgb;
  }

  vec3 metallicRoughness = texture(metallicRoughnessTexture, TexCoords).rgb;
  float roughness = metallicRoughness.g;
  float metallic = metallicRoughness.b;

  vec3 V = normalize(camPos - WorldPos);
  vec3 L = lightDirection;
  vec3 H = normalize(V + L);
  vec3 radiance = lightColour * 4;

  vec3 N;

  // Normal mapping
  if (hasNormalTexture) {
    N = texture(normalTexture, TexCoords).rgb * normalTextureScale;
    N = N * 2.0 - 1.0;
    N = normalize(TBN * N);
  } else {
    N = Normal;
  }

  if (doubleSided && dot(N, V) < 0.0)
    N = -N;

  // Cook-Torrance BRDF
  float NDF = DistributionGGX(N, H, roughness);
  float G   = GeometrySmith(N, V, L, roughness);
  vec3 F0 = mix(vec3(0.04), baseColor, metallic);
  vec3 F    = fresnelSchlick(max(dot(H, V), 0.0), F0);

  vec3 kS = F;
  vec3 kD = vec3(1.0) - kS;
  kD *= 1.0 - metallic;

  vec3 numerator    = NDF * G * F;
  float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.0001;
  vec3 specular     = numerator / denominator;

  float NdotL = max(dot(N, L), 0.0);
  float shadow = ShadowCalculation(LightClipPos);
  vec3 Lo = (1 - shadow) * (kD * baseColor / PI + specular) * radiance * NdotL;

  // TODO Ambient occlusion
  vec3 ambient = ambientIntensity * lightColour * baseColor;// * ao;

  vec3 lighting = ambient + Lo;

  // Tone mapping
  lighting = lighting / (lighting + vec3(1.0));

  // Gamma correction
  float gamma = 2.2;
  lighting = pow(lighting, vec3(1.0/gamma));

  FragColor = vec4(lighting, 1.0);
}
