#version 460 core
out vec4 FragColor;

in vec4 LightClipPosition;
in vec3 Normal; // In view space
in vec3 WorldPosition;

uniform sampler2D lightDepthMap;

uniform float ambientIntensity;
uniform vec3 lightDirection;
uniform mat4 viewM;

float ShadowCalculation(vec4 clipPos)
{
    // remove shadow acne
    float bias = max(0.05 * (1.0 - dot(Normal, lightDirection)), 0.005);

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
      for(int x = -1; x <= 1; ++x)
      {
        for(int y = -1; y <= 1; ++y)
        {
          float pcfDepth = texture(lightDepthMap, mapCoords.xy + vec2(x, y) * texelSize).r;
          shadow += currentDepth - bias > pcfDepth ? 1.0 : 0.0;
        }
      }
      shadow /= 9.0;
    }
    return shadow;
}  

void main()
{
    vec3 objectColour = vec3(1.0f, 1.0f, 1.0f);
    vec3 lightColour = vec3(1.0f, 1.0f, 1.0f);
    // Point light source
    // Unidirectional light source (e.g. sunlight)
    vec3 lightViewDirection = normalize(mat3(viewM) * lightDirection);
    // Ambient light
    vec3 ambient = ambientIntensity * lightColour;
    // Diffuse light
    vec3 diffuse = max(dot(Normal, lightViewDirection), 0.0f) * lightColour;
    // Specular lighting
    /*
    float specularIntensity = 1;
    vec3 viewDirection = normalize(-WorldPosition);
    vec3 reflectDirection = reflect(-lightViewDirection, Normal); 
    vec3 specular = specularIntensity
      * pow(max(dot(viewDirection, reflectDirection), 0.0), 128) * lightColour;  
    */
    vec3 specular = vec3(0.0, 0.0, 0.0);
    // Shadow
    float shadow = ShadowCalculation(LightClipPosition);

    vec3 lighting = ambient + (1.0 - shadow) * (diffuse + specular) * objectColour;
    FragColor = vec4(lighting, 1.0);
} 
