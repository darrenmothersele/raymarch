#version 330 core

uniform float time;
uniform vec2 resolution;
uniform vec4 mouse;

out vec4 fragColor;

#define PI 3.1415926535898 // Always handy.

// Epsilon value..
const float eps = 0.005;

// Gloable variables for the raymarching algorithm.
const int maxIterations = 128; // The framerate stay up with higher numbers, but just to be on the safe side...
const int maxIterationsRef = 32; // Higher numbers are always more accurate, but if I can get away with it, I'll drop this to 16.
const int maxIterationsShad = 32;
const float stepScale = 0.5; // If you lower the iterations, you'll usually have to increase step distance.
const float stepScaleRef = 0.95;
const float stopThreshold = 0.01; // I'm not quite sure why, but thresholds in the order of a pixel seem to work better for me... most times.

// Globals.
vec3 lp = vec3(0.);
vec3 surfNormal = vec3(0.);
vec3 ref = vec3(0.);
vec3 lookAt = vec3(0.);
vec3 camPos = vec3(0.);

// The following are pretty handy for all sorts of situations, but in this case, they're
// used for rotating camera.

// 2x2 matrix rotation.
mat2 rot2( float angle ){
	float c = cos( angle );
	float s = sin( angle );

	return mat2(
		 c, s,
		-s, c
	);
}

float planeXZ(in vec3 p, float dist) {

	return p.y - dist;
}

// Distance field equation for a sphere.
float sphere(in vec3 p, in vec3 centerPos, float radius){

	return length(p-centerPos) - radius;
}

// Distance field equation for a cube.
float box(vec3 p, vec3 b){

	vec3 d = abs(p) - b;

	return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
}

// Distance field equation for a rounded cube.
float roundedCube(vec3 p, vec3 boxExtents, float edgeRadius ){

	return length(max(abs(p)-boxExtents + vec3(edgeRadius), 0.0 )) - edgeRadius;
}

// Sinusoidal plasma.
float sinusoidalBumps(in vec3 p){

    return sin(p.x*4.+time*0.97)*cos(p.y*4.+time*2.17)*sin(p.z*4.-time*1.31) + 0.5*sin(p.x*8.+time*0.57)*cos(p.y*8.+time*2.11)*sin(p.z*8.-time*1.23);
}

// There's a little science in amongst this lot, but for the most part, the following was trial and error. It's more than likely to degrade
// when using large input values, but for this particular example, that shouldn't be a problem.
vec3 hashCheapAnimate(vec3 p){

    //p = sin(p); // A hack to ease degradation when using large input values.

    return cos(vec3( 174.3637*p.y*p.z + 13.53*p.x*(p.x-123.667),
                     219.3379*p.x*p.z + 11.79*p.y*(p.y-119.365),
                     223.3261*p.x*p.y + 13.87*p.z*(p.z-113.913)
                 ) + vec3(0.93,1.17, 1.01)*time)*0.5;

}

// Obviously, this is just a slightly-trimmed rehash of IQ's (iquilezles.org) original code.
//
// I wanted some relatively cheap, animated, 3D Voronoi, and this was the best I could come up with in a short amount of time -
// without resorting to an 8-Tap version (much faster, but less visually appealing) or a precalc texture. Unrolling another loop
// can speed things up, but I've sacrificed speed for readability.
float cheapAnimateVoronoi(vec3 uv) {

	vec3 g = floor(uv);
	vec3 f = fract(uv);

	vec3 b = vec3(0.), o = b, r = b;

	float d = 0.36; // The highest possible maximum is sqrt(3), by the way.

	for(float j = -0.5; j < 1.51; j++) {
	    for(float i = -0.5; i < 1.51; i++) {

		    b = vec3(i, j, -0.5);
		    r = b - f + hashCheapAnimate(g+b);
		    d = min(d, dot(r,r));

		    b.z = 0.5;
		    r = b - f + hashCheapAnimate(g+b);
		    d = min(d, dot(r,r));

		    b.z = 1.5;
		    r = b - f + hashCheapAnimate(g+b);
		    d = min(d, dot(r,r));

	    }
	}

	return d/1.36; // Square roots, and so forth, are left to the discretion of the user.
}

float smoothFract(float x){

    float f = fract(x);

    float f1 = min(f*(1.-f)*10., f);

    return f1;//pow(f1, 0.8);

}

// The bump mapping function.
float bumpFunction(in vec3 p){

     if(p.y>-0.8) return sqrt(cheapAnimateVoronoi(p*16.));
     else return sqrt(cheapAnimateVoronoi(p*4.));

}

// Bump mapping.
vec3 doBumpMap( in vec3 p, in vec3 nor, float bumpfactor ){

	float ref = bumpFunction( p );
	// Note: To save on calculations, we're stepping to just one side of the position "p," rather than both.
    vec3 grad = vec3( bumpFunction(vec3(p.x+eps, p.y, p.z))-ref,
                      bumpFunction(vec3(p.x, p.y+eps, p.z))-ref,
                      bumpFunction(vec3(p.x, p.y, p.z+eps))-ref )/eps;

    // I tend to favor subtle bump mapping, so since this line has a subtle effect already, I have a bad habit of leaving it out.
    // However, it should definitely be there, so if you do notice it missing in my other work, send me an email letting me know
    // what a lazy dumbass I am. :)
    grad -= nor*dot(grad, nor);

    return normalize( nor - bumpfactor*grad );

}

// This is the raymarched scene that gets called multiple times from the raymarching procedure, which means adding detail can slow things right down.
float scene(in vec3 p){

    // In this case, the displacement mapping is provided via a cheap sinusoidal function, but more interesting variations
    // can be created using textures.

    float distortedObject = sphere(p, vec3(0., 1.1 , 2.5), 1.) + 0.3*sinusoidalBumps(p);
    //float distortedObject = roundedCube(p-vec3(0.,1.1,2.5), vec3(0.75,0.75,0.75), 0.15) + 0.3*sinusoidalBumps(p);
	float distortedFloor = planeXZ(p, -1.0) + 0.1*sinusoidalBumps(p*0.5);

	return min(distortedObject, distortedFloor);
}

// Obtain the surface normal at the surface point "p."
vec3 getNormal(in vec3 p) {

	return normalize(vec3(
		scene(vec3(p.x+eps,p.y,p.z))-scene(vec3(p.x-eps,p.y,p.z)),
		scene(vec3(p.x,p.y+eps,p.z))-scene(vec3(p.x,p.y-eps,p.z)),
		scene(vec3(p.x,p.y,p.z+eps))-scene(vec3(p.x,p.y,p.z-eps))
	));

}

// Raymarching.
float rayMarching( vec3 origin, vec3 dir, float start, float end ) {

    float sceneDist = 1e4;
	float rayDepth = start; // Ray depth. "start" is usually zero, but for various reasons, you may wish to start the ray further away from the origin.
	for ( int i = 0; i < maxIterations; i++ ) {
		sceneDist = scene( origin + dir * rayDepth ); // Distance from the point along the ray to the nearest surface point in the scene.

        // Irregularities between browsers have forced me to use this logic. I noticed that Firefox was interpreting two "if" statements inside a loop
        // differently to Chrome, and... 20 years on the web, so I guess I should be used to this kind of thing.
		if (( sceneDist < stopThreshold ) || (rayDepth >= end)) {

		    // (rayDepth >= end) - The casted ray has proceeded past the end zone, so it's time to return the maximum distance.

		    // (sceneDist < stopThreshold) - The distance is pretty close to zero, which means the point on the ray has effectively come into contact
		    // with the surface. Therefore, we can return the distance, which can be used to calculate the surface point.

			break;
		}
		// We haven't hit anything, so increase the depth by a scaled factor of the minimum scene distance.
		rayDepth += sceneDist * stepScale;

	}

	// I'd normally arrange for the following to be taken care of prior to exiting the loop, but Firefox won't execute anything before
	// the "break" statement. Why? I couldn't say. I'm not even game enough to put more than one return statement.
	if ( sceneDist >= stopThreshold ) rayDepth = end;
	else rayDepth += sceneDist;

	// We've used up our maximum iterations. Return the maximum distance.
	return rayDepth;
}

// Raymarching reflections.
float rayMarchingReflections( vec3 origin, vec3 dir, float start, float end ) {

	float sceneDist = 1e4;
	float rayDepth = start; // Ray depth. "start" is usually zero, but for various reasons, you may wish to start the ray further away from the origin.
	for ( int i = 0; i < maxIterationsRef; i++ ) {
		sceneDist = scene( origin + dir * rayDepth ); // Distance from the point along the ray to the nearest surface point in the scene.

		if (( sceneDist < stopThreshold ) || (rayDepth >= end)) {

		    // (rayDepth >= end) - The casted ray has proceeded past the end zone, so it's time to return the maximum distance.

		    // (sceneDist < stopThreshold) - The distance is pretty close to zero, which means the point on the ray has effectively come into contact
		    // with the surface. Therefore, we can return the distance, which can be used to calculate the surface point.

			break;
		}

		// We haven't hit anything, so increase the depth by a scaled factor of the minimum scene distance.
		rayDepth += sceneDist * stepScaleRef;

	}


	// I'd normally arrange for the following to be taken care of prior to exiting the loop, but Firefox won't execute anything before
	// the "break" statement. Why? I couldn't say. I'm not even game enough to put more than one return statement.
	if ( sceneDist >= stopThreshold ) rayDepth = end;
	else rayDepth += sceneDist;

	// We've used up our maximum iterations. Return the maximum distance.
	return rayDepth;
}

// Based on original by IQ - optimized to remove a divide.
float calculateAO(vec3 p, vec3 n)
{
   const float AO_SAMPLES = 5.0;
   float r = 0.0;
   float w = 1.0;
   for (float i=1.0; i<=AO_SAMPLES; i++)
   {
      float d0 = i * 0.2; // 1.0/AO_SAMPLES
      r += w * (d0 - scene(p + n * d0));
      w *= 0.5;
   }
   return 1.0-clamp(r,0.0,1.0);
}

// I wrote my own soft-shadow code, but I didn't like it as much as I like this snippet. It's nice and simple, relatively fast, and
// gives decent results. I guess that's why so many people use it.
//
// You can read more about the following at: http://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm
float softShadow(vec3 ro, vec3 rd, float start, float end, float k){

    float shade = 1.0;

    // The "start" value, or minimum, should be set to something more than the stop-threshold, so as to avoid a collision with
    // the surface the ray is setting out from. It doesn't matter how many times I write shadow code, I always seem to forget this.
    // If adding shadows seems to make everything look dark, that tends to be the problem.
    float dist = start;
    float stepDist = end/float(maxIterationsShad);

    // Max shadow iterations - More iterations make nicer shadows, but slow things down. Obviously, the lowest
    // number to give a decent shadow is the best one to choose.
    for (int i=0; i<maxIterationsShad; i++){
        // End, or maximum, should be set to the distance from the light to surface point. If you go beyond that
        // you may hit a surface not between the surface and the light.
        float h = scene(ro + rd*dist);
        shade = min(shade, k*h/dist);

        // What h combination you add to the distance depends on speed, accuracy, etc. To be honest, I find it impossible to find
        // the perfect balance. Faster GPUs give you more options, because more shadow iterations always produce better results.
        // Anyway, here's some posibilities. Which one you use, depends on the situation:
        // +=h, +=clamp( h, 0.01, 0.25 ), +=min( h, 0.1 ), +=stepDist, +=min(h, stepDist*2.), etc.
        dist += min(h, stepDist*2.); // The best of both worlds... I think.

        // Early exits from accumulative distance function calls tend to be a good thing.
        if (h<0.001 || dist > end) break;
    }

    // I've added 0.2 to the final shade value, which lightens the shadow a bit. It's a preference thing. Really dark shadows look
    // too brutal to me.
    return min(max(shade, 0.) + 0.2, 1.0);
}

vec3 lighting( vec3 sp, vec3 camPos, int reflectionPass){

    // Start with black.
    vec3 sceneColor = vec3(0.0);

    // Object's color.
    vec3 voxPos = mod(sp*0.5, 1.0);

    // Sometimes, I like to accentuate the bump map effect by directly coloring the object with the bump map function. It's
    // almost standard when using textures. Comment it out to see the difference.
    vec3 objColor = vec3(1.0, 1.0, 1.0);
	// Shading/coloring for the object. You can comment the two lines out with no consequence.
	float bumps =  bumpFunction(sp);//*(1.0-0.65*sinusoidalBumps(sp));
	vec3 tinge = vec3(bumps)*vec3(0.8+0.05*sin(sp.z*8.), 0.95+0.05*sin(sp.y*16.), 1.0);
	if(sp.y>-0.8)tinge = bumps*vec3(0.2+0.1*sin(sp.z*16.), 1.0, 0.7+0.1*sin(sp.y*8.)); //

    objColor = clamp(objColor*tinge, 0.0, 1.0);


    // Obtain the surface normal at the scene position "sp."
    surfNormal = getNormal(sp);

    // Bumpmapping the surface nomral. Note that we don't bother with this during the reflections pass, which is not entirely accurate
    // but not all that noticable.
    if (reflectionPass==0)surfNormal = doBumpMap(sp, surfNormal, 0.04); //Bump factor changes with detail. Usually, the more detail an image has, the less bump it will need.


    // Lighting.

    // lp - Light position. Keeping it in the vacinity of the camera, but away from the objects in the scene.
    //lp = vec3(camPos.x, 0.75, camPos.z);
    // ld - Light direction.
    vec3 ld = lp-sp;
    // lcolor - Light color.
    vec3 lcolor = vec3(1.,0.97,0.92);

     // Light falloff (attenuation).
    float len = length( ld ); // Distance from the light to the surface point.
    ld /= len; // Normalizing the light-to-surface, aka light-direction, vector.
    float lightAtten = min( 1.0 / ( 0.05*len*len ), 1.0 ); // Keeps things between 0 and 1.


    // Obtain the reflected vector at the scene position "sp."
    ref = reflect(-ld, surfNormal);

    // Shadows can be expensive, so although not entirely accurate, I leave this out of the refection lighting. Hence the "if" statement.
    float shadowcol = 1.0;
    if (reflectionPass==0)shadowcol=softShadow(sp, ld, stopThreshold*2.0, len, 16.0); // Be careful not to let the ray hit the object upon initiation.

    // Ambient occlusion. It's subtle for this particular scene, but can be great under the right circumstances. I'll explain it later.
    float ao = 0.5+0.5*calculateAO(sp, surfNormal); // Ambient occlusion.

    float ambient = .1; //The object's ambient property.
    float specularPower = 16.0; // The power of the specularity. Higher numbers can give the object a harder, shinier look.
    float diffuse = max( 0.0, dot(surfNormal, ld) ); //The object's diffuse value.
    float specular = max( 0.0, dot( ref, normalize(camPos-sp)) ); //The object's specular value.
    specular = pow(specular, specularPower); // Ramping up the specular value to the specular power for a bit of shininess.

    // Bringing all the lighting components togethr to color the screen pixel.
    sceneColor += (objColor*(diffuse*0.9+ambient)+specular*0.5)*lcolor*lightAtten*shadowcol*ao;

    return sceneColor;

}


void main(void) {


    // Setting up our screen coordinates.

    vec2 aspect = vec2(resolution.x/resolution.y, 1.0); //
	vec2 screenCoords = (2.0*gl_FragCoord.xy/resolution.xy - 1.0)*aspect;


	// Camera Setup.

	// Camera movement.
	vec3 lookAt = vec3(0.,1.1,2.5);  // This is the point you look towards, or at, if you prefer.
	vec3 camPos = vec3(0.+3.25*cos(time*0.125), 2., 2.5+3.25*sin(time*0.125)); // This is the point you look from, or camera you look at the scene through. Whichever way you wish to look at it.

    // lp - Light position. You'll note that I've made it global, because it's also used in the light function. I find it easier to keep it near the camera setup, but
    // that's a preference thing. You could put it back in the light function. I'm placing the light in the vacinity of the camera.
    lp = vec3(camPos.x+0.25, camPos.y+2.0, camPos.z+0.5);

    vec3 forward = normalize(lookAt-camPos); // Forward vector.
    vec3 right = normalize(vec3(forward.z, 0., -forward.x )); // Right vector... or is it left? Either way, so long as the correct-facing up-vector is produced.
    vec3 up = normalize(cross(forward,right)); // Cross product the two vectors above to get the up vector.

    // FOV - Field of view.
    float FOV = 0.5;

    // ro - Ray origin.
    vec3 ro = camPos;
    // rd - Ray direction.
    vec3 rd = normalize(forward + FOV*screenCoords.x*right + FOV*screenCoords.y*up);



	// The screen's background color.
    vec3 bgcolor = vec3(0.);


	// Ray marching.
	const float clipNear = -1.0;
	const float clipFar = 16.0;
	float dist = rayMarching(ro, rd, clipNear, clipFar );
	if ( dist >= clipFar ) {
	    // I prefer to do it this way in order to avoid an if-statement below, but I honestly couldn't say whether it's more
	    // efficient. It feels like it would be. Does that count? :)
	    fragColor = vec4(bgcolor, 1.0);
	    return;
		//discard; // If you want to return without rendering anything, I think.
	}

	// sp - Surface position. If we've made it this far, we've hit something.
	vec3 sp = ro + rd*dist;

	// Light the pixel that corresponds to the surface position. The last value "0" tells the lighting function whether to include
	// shadows, or not. Shadows can be expensive, so I tend not to include them on the original pass, and not include them during
	// the reflection pass, which is slightly less accurate, but not really noticable.
	vec3 sceneColor = lighting( sp, camPos, 0);


#ifdef REFLECTION

    // Reflection

	// We've completed the first surface collision pass, so now we can begin the reflected ray pass.
	dist = rayMarchingReflections(sp, reflect(rd, surfNormal), stopThreshold*5.0, clipFar );
	vec3 rsp = sp + ref*dist;

	if ( dist >= clipFar ) {
	    // It'd look cleaner, if I simply changed the sign above, and put the lighting portion in here, but I'm under the
	    // impression that GPUs aren't fond of nesting, so I'm doing it in a less tidy way.
	    fragColor = vec4(clamp(sceneColor, 0.0, 1.0), 1.0);
	    return;
	}

	// The reflected ray hit something, so light the "reflected" pixel that corresponds to the "reflected" surface position.
	// The last entry "1" tells the lighting function to not include shadows, which are less important during a reflection pass.
    float refCoef = 0.35; // Reflective coefficient. The amount of reflected light we wish to incorporate into the final color.
    sceneColor += lighting( rsp, sp, 1)*refCoef;

#endif

    // Clamping the lit pixel, then put it on the screen.
	fragColor = vec4(clamp(sceneColor, 0.0, 1.0), 1.0);

}
