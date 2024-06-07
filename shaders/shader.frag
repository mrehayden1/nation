#version 460 core
in vec4 LightClipPos;
in vec3 Normal;
in mat3 TBN;
in vec4 WorldPos;
in vec2 TexCoords;

out vec4 FragColor;

layout (binding = 0) uniform sampler2D lightDepthMap;
layout (binding = 1) uniform sampler2D baseColorTexture;
layout (binding = 2) uniform sampler2D metallicRoughnessTexture;
layout (binding = 3) uniform sampler2D normalTexture;

uniform bool hasBaseColorTexture;
uniform vec4 baseColorFactor;
uniform int alphaMode;
uniform float alphaCutoff;

uniform bool hasMetallicRoughnessTexture;
uniform float metallicFactor;
uniform float roughnessFactor;

uniform bool hasNormalTexture;
uniform float normalTextureScale;

uniform vec3 daylightColor;
uniform float daylightIntensity;
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
  vec3 lightColour = daylightColor * daylightIntensity;

  vec4 baseColor;

  if (hasBaseColorTexture) {
    baseColor = texture(baseColorTexture, TexCoords) * baseColorFactor;
  } else {
    baseColor = baseColorFactor;
  }

  // Alpha coverage
  if (alphaMode == ALPHA_MODE_MASK) {
    if (baseColor.a < alphaCutoff) {
      discard;
    } else {
      baseColor.a = 1.0f;
    }
  } else if (alphaMode == ALPHA_MODE_OPAQUE) {
    baseColor.a = 1.0f;
  }

  float roughness, metallic;

  if (hasMetallicRoughnessTexture) {
    vec3 metallicRoughness = texture(metallicRoughnessTexture, TexCoords).rgb;
    roughness = metallicRoughness.g;
    metallic = metallicRoughness.b;
  } else {
    roughness = roughnessFactor;
    metallic = metallicFactor;
  }

  vec3 V = normalize(camPos - vec3(WorldPos));
  vec3 L = lightDirection;
  vec3 H = normalize(V + L);
  vec3 radiance = lightColour * 4.0;

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
  vec3 F0 = mix(vec3(0.04), baseColor.rgb, metallic);
  vec3 F    = fresnelSchlick(max(dot(H, V), 0.0), F0);

  vec3 kS = F;
  vec3 kD = vec3(1.0) - kS;
  kD *= 1.0 - metallic;

  vec3 numerator    = NDF * G * F;
  float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.0001;
  vec3 specular     = numerator / denominator;

  float NdotL = max(dot(N, L), 0.0);
  float shadow = ShadowCalculation(LightClipPos);
  vec3 Lo = (1 - shadow) * (kD * baseColor.rgb / PI + specular) * radiance
              * NdotL;

  vec3 ambient = daylightIntensity * lightColour * baseColor.rgb;// * ao;

  vec3 lighting = 10 * ambient + 5 * Lo;

  // Exposure
  float exposure = 0.7;
  lighting = lighting * exposure;

  // TODO White balance

  // Reinhard tone mapping
  lighting = lighting / (lighting + vec3(1.0));

  // Contrast
  float contrast = 0.95;
  float brightness = -0.05;
  lighting = contrast * (lighting - 0.5) + 0.5 + brightness;

  // Saturation
  float saturation = 1.1;
  float greyscale = dot(lighting, vec3(0.299, 0.587, 0.114));
  lighting = mix(vec3(greyscale), lighting, saturation);

  // Gamma correction
  float gamma = 2.2;
  lighting = pow(lighting, vec3(1.0/gamma));

  FragColor = vec4(lighting, baseColor.a);
}
