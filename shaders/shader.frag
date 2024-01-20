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
    // TODO Improved bias calculation
    float bias = 0.005; // remove shadow acne

    // perform perspective divide
    vec3 ndcCoords = clipPos.xyz / clipPos.w;
    // transform to [0,1] range
    vec3 mapCoords = ndcCoords * 0.5 + 0.5;
    // get closest depth value from light's perspective (using [0,1] range fragPosLight as coords)
    float closestDepth = texture(lightDepthMap, mapCoords.xy).r; 
    // get depth of current fragment from light's perspective
    float currentDepth = mapCoords.z;
    // check whether current frag pos is in shadow
    float shadow = currentDepth - bias > closestDepth ? 1.0 : 0.0;

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
