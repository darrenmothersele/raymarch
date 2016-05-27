#version 330 core

uniform float time;
uniform vec2 resolution;
uniform vec4 mouse;

out vec4 fragColor;

//varying vec2 surfacePosition;

// Raymarching Template
// Source - Raymarching.com
// Author - Gary "Shane" Warne
// eMail - mail@Raymarching.com, mail@Labyrinth.com
// Last update: 28th Aug, 2014
//
// All of the code on this page has been written a gazillion times by a zillion different people. There is nothing new here. Screen coordinates,
// camera setup, ray setup, raymarch, lighting, screen render, and done. Use it for whatever you purpose you like.
//
// Anyway, I'm genuinely grateful for all the code various people have made public over at "Shadertoy.com," which is run by that RGBA guy, Inigo Quilez,
// aka IQ. "Shader Toy" is an apt description for that site, because as a person who loves graphic-related snippets, I'm like a kid in a toy store
// over there. I guess you needed to have been on the net 15, or even 10, years ago trying to hunt down demo effects code to really appreciate what's
// being offered at "ShaderToy.com." Fantastic stuff.

#define PI 3.1415926535898 // Always handy.

// Epsilon value..
const float eps = 0.005;

// Gloable variables for the raymarching algorithms.
const int maxIterations = 128;
const int maxIterationsRef = 32;
const int maxIterationsShad = 24;
const float stepScale = 0.5;
const float stepScaleRef = 0.95;
const float stopThreshold = 0.005; // I'm not quite sure why, but thresholds in the order of a pixel seem to work better for me... most times.

// We use the light position in different areas, as well as the surface normal, so I've sacrificed readability and globalized them to save repetition.
// GPUs are lightning fast these days, but once you start trying to include shadows, reflections, ambient occlusion, and so forth, things start slowing
// down, which means you have to go back to saving cycles here and there. Writing code under time constaints that's feature-rich, short, readable,
// robust and fast? That'd be nice. It never happens to me, but it would be nice. :)
vec3 lp = vec3(0.);
vec3 surfNormal = vec3(0.);
vec3 ref = vec3(0.);

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
float sinusoidalPlasma(in vec3 p){

    return sin(p.x+time*2.)*cos(p.y+time*2.1)*sin(p.z+time*2.3) + 0.25*sin(p.x*2.)*cos(p.y*2.)*sin(p.z*2.);
}

// This is the raymarched scene that gets called multiple times from the raymarching procedure, which means adding detail can slow things right down.
float scene(in vec3 p) {

    // The following is probably one of my favorite raymarching moves. This tiny snippet is responsibe for the infinite object repetition that
    // you see in so many raymarched scenes. There are a lot of different ways to look at it, but in a way, you're voxelizing the entire 3D space
    // into cubes of one unit dimension, then placing a copy of your object (in this case, a box) right in the center of it. Obviously, the
    // object you place in it has to fit, and you'll see that the box below does. By the way, no one's forcing you to voxelize to 1 unit dimension.
    // For instance, "p = mod(p, 2.0) - 1.0" would work also, but your boxes would be twice as far apart, and appear darker, since your light would
    // now be further away. Try it and see.
    p = mod(p, 1.0) - 0.5;

    // In addition to the box field equation, I've also provided the distance functions for a sphere and rounded cube. I'll assume these particular
    // ones originates at "iquilezles.org," although anyone who's ever implemented the marching cubes algorithm would be familiar with them.

    float d0 = roundedCube(p, vec3(0.2,0.15,0.25), 0.05); // Smaller edge radius mean boxier look - (0.->box, outer-extremites->more spherical).
    //float d0 = sphere(p, vec3(0.,0.,0.), 0.25); // Sphere. Boring, yet cool at the same time.
    //float d0 = box(p, vec3(0.2, 0.15, 0.25)); // Box. As above.
    //float d1 = box(p, vec3(0.22, 0.075, 0.125));

    // You'll note that I've commented out a term that gives the boxes some sinusoidal movement. It's kind of cool, but detracts from the overall look.
    // Sometimes, less is more, as they say.
	return d0;// + 0.03*sinusoidalPlasma(p*8.0);//+ .01-0.02*sinusoidalPlasma(p*2.0);
}

// Obtain the surface normal at the surface point "p."
vec3 getNormal(in vec3 p) {

	return normalize(vec3(
		scene(vec3(p.x+eps,p.y,p.z))-scene(vec3(p.x-eps,p.y,p.z)),
		scene(vec3(p.x,p.y+eps,p.z))-scene(vec3(p.x,p.y-eps,p.z)),
		scene(vec3(p.x,p.y,p.z+eps))-scene(vec3(p.x,p.y,p.z-eps))
	));

    /*
    // Shorthand version of the above. The fewer characters used almost gives the impression that it involves fewer calculations. Almost.
	vec2 e = vec2(eps, 0.);
	return normalize(vec3(scene(p+e.xyy)-scene(p-e.xyy), scene(p+e.yxy)-scene(p-e.yxy), scene(p+e.yyx)-scene(p-e.yyx) ));
    */

	/*
	// The tetrahedral version, which does involve fewer calculations, but doesn't seem as accurate on some surfaces... I could be wrong,
	// but that's the impression I get.
	vec2 e = vec2(-0.5*eps,0.5*eps);
	return normalize(e.yxx*scene(p+e.yxx)+e.xxy*scene(p+e.xxy)+e.xyx*scene(p+e.xyx)+e.yyy*scene(p+e.yyy));
	*/
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
	//
	// Normally, you'd just return the rayDepth value only, but for some reason that escapes my sense of logic - and everyone elses, for
	// that matter, adding the final, infinitessimal scene distance value (sceneDist) seems to reduce a lot of popping artifacts. If
	// someone could put me out of my misery and prove why I should either leave it there, or get rid of it, it'd be appreciated.
	if ( sceneDist >= stopThreshold ) rayDepth = end;
	else rayDepth += sceneDist;

	// We've used up our maximum iterations. Return the maximum distance.
	return rayDepth;
}

// Raymarching reflections. It appears that GPUs won't do loops with variable iterations, but reflections are expensive, so you need to use fewer
// iterations. Therefore, I've had to make an almost duplicate version of the raymarching function above. Surely, there's a better way, but at least
// it works. Reflection are a little fiddly, but otherwise easy to implement. Unfortunately, they take up extra iterations that, sometimes, your poor
// GPU can't handle.
//
// Anyway, once you've hit a surface point in the scene, the surface point will become the new origin, and the normalized reflected vector
// will become the new ray direction (dir). Feed those into the function below, then use the resultant distance to obtain the surface the reflected
// ray hits (if any). Put that result into the light equation, then add a portion of the color to the color you've already attained from the
// first raymarching pass. Simple... once you've done it a few times and get used to the process.
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
	//
	// Normally, you'd just return the rayDepth value only, but for some reason that escapes my sense of logic - and everyone elses, for
	// that matter, adding the final, infinitessimal scene distance value (sceneDist) seems to reduce a lot of popping artifacts. If
	// someone could put me out of my misery and prove why I should either leave it there, or get rid of it, it'd be appreciated.
	if ( sceneDist > stopThreshold ) rayDepth = end;
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
// In essence, a surface point is in shadow when an object gets in the way of the light. For hard shadows, you cast a ray from
// the surface point (just a little off the surface, to be accurate) to the light position, and if you return a hit, you darken
// the pixel. The following does something similar, but takes advantage of the fact that you're using a raymarching technique.
//
// Instead of only considering whether the point you're shading has another surface directly between it and the light, you consider
// all points along the ray by testing how close each point comes within an object on the way to the light (h), and how far the
// point on the ray is from your original cast off point (dist). The value "k" is just a fade-off factor that enables you to control
// how soft you want the shadows to be. Smaller values give a softer penumbra, and larger values give a more hard edged shadow.
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

    // I've added 0.3 to the final shade value, which lightens the shadow a bit. It's a preference thing. Really dark shadows look
    // too brutal to me.
    return min(max(shade, 0.) + 0.3, 1.0);
}

vec3 lighting( vec3 sp, vec3 camPos, int reflectionPass){

    // Start with black.
    vec3 sceneColor = vec3(0.0);

    // Object's color.
    vec3 voxPos = mod(sp*0.5, 1.0);
    vec3 objColor = vec3(1.0, 1.0, 1.0)*0.975 + sin(voxPos*8.0)*0.05;
    if ( (voxPos.x<0.5)&&(voxPos.y>=0.5)&&(voxPos.z>=0.5) ) objColor = 0.5+0.5*sin(voxPos*1.625*PI);
    else if ( (voxPos.x>=0.5)&&(voxPos.y<0.5)&&(voxPos.z<0.5) ) objColor = 0.5+0.5*sin(voxPos*1.625*PI);
    //if ( (voxPos.x<0.5)&&(voxPos.y>=0.5)&&(voxPos.z<0.5) ) objColor = vec3(1.0,voxPos.z*0.5,0.0);
    //else if ( (voxPos.x>=0.5)&&(voxPos.y<0.5)&&(voxPos.z>=0.5) ) objColor = vec3(0.0,0.5+voxPos.z*0.5,1.0);

    objColor = clamp(objColor, 0.0, 1.0);

    //float checkSize = 1.0;
    //if ( mod(floor(checkSize * sp.x) + floor(checkSize * sp.y) + floor(checkSize *sp.z), 2.0) < 1.0 ) objColor = vec3(1.0,0.1,0.0);

    //objColor = clamp(objColor*(0.75-0.25*sinusoidalPlasma(sp*8.), 0.0, 1.0);


    // Obtain the surface normal at the scene position "sp."
    surfNormal = getNormal(sp);

    // Lighting.

    // lp - Light position. Keeping it in the vacinity of the camera, but away from the objects in the scene.
    lp = vec3(0.25*sin(time), 0.75, 0.25*cos(time)+time);
    // ld - Light direction.
    vec3 ld = lp-sp;
    // lcolor - Light color.
    vec3 lcolor = vec3(1.,0.98,0.95);

     // Light falloff (attenuation).
    float len = length( ld ); // Distance from the light to the surface point.
    ld /= len; // Normalizing the light-to-surface, aka light-direction, vector.
    float lightAtten = min( 1.0 / ( 0.25*len*len ), 1.0 ); // Keeps things between 0 and 1.

    // Obtain the reflected vector at the scene position "sp."
    ref = reflect(-ld, surfNormal);

    // Shadows can be expensive, so although not entirely accurate, I leave this out of the refection lighting. Hence the "if" statement.
    float shadowcol = 1.0;
    if (reflectionPass==0)shadowcol=softShadow(sp, ld, stopThreshold*2.0, len, 32.0); // Be careful not to let the ray hit the object upon initiation.

    // I decided to keep ambient occlusion out of this one, but I'll explain it later. Uncomment it, if you want to include it.
    float ao = 1.0; //0.5+0.5*calculateAO(sp, surfNormal); // Ambient occlusion.

    float ambient = .05; //The object's ambient property.
    float specularPower = 8.0; // The power of the specularity. Higher numbers can give the object a harder, shinier look.
    float diffuse = max( 0.0, dot(surfNormal, ld) ); //The object's diffuse value.
    float specular = max( 0.0, dot( ref, normalize(camPos-sp)) ); //The object's specular value.
    specular = pow(specular, specularPower); // Ramping up the specular value to the specular power for a bit of shininess.

    // Bringing all the lighting components togethr to color the screen pixel.
    sceneColor += (objColor*(diffuse*0.8+ambient)+specular*0.5)*lcolor*lightAtten*shadowcol*ao;

    return sceneColor;

}


void main(void) {


    // Setting up our screen coordinates.

    vec2 aspect = vec2(resolution.x/resolution.y, 1.0); //
	vec2 screenCoords = (2.0*gl_FragCoord.xy/resolution.xy - 1.0)*aspect;


	// Camera Setup.

	// Camera movement. Not my best work, and there are better ways to do this, but at least it gives the viewer a bit of a look around the scene.
	// The main thing to note is that the camera (and position we're looking at) is being moved forward, linearly, along the z-axis. At the same time,
	// the camera is circling the view point at a radius of one unit about the xz-plane. Then, just to confuse everyone even more, I seem to
	// have decided to send both the camera and the look-at positions on different sinusoidal paths along the y-axis... I'm sure I had my reasons. Either way,
	// the scene now has a moving camera.
	vec3 lookAt = vec3(0., 1.*sin(time*0.5), time);  // This is the point you look towards, or at, if you prefer.
	vec3 camPos = vec3(1.0*sin(time*0.5), 0.15*sin(time*0.25), 1.0*cos(time*0.5)+time); // This is the point you look from, or camera you look at the scene through. Whichever way you wish to look at it.

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
	const float clipNear = 0.0;
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

	// Reflection

	// We've completed the first surface collision pass, so now we can begin the reflected ray pass. It's done in the same way
	// as above, except that our origin is now the point on the surface of the object we've just hit (sp), and the ray direction
	// (rd) is simply the reflected ray (ref). If we construct a vector from the light to the surface postion, the reflected
	// ray will be the ray cast off in the mirror reflection across the surface normal - A diagram would be helpful right about
	// now, but I'll probably write about this later. For now, just look one up on the net.
	//
	// By the way, in theory, we're not restricted to just one reflection pass. We could do this multiple times, by obtaining the
	// reflected ray of the reflected ray, and so forth. Unfortunately, modern GPUs have their limits, so just the one pass
	// will have to suffice. It'd be nice to have more lights too, but that means even more passes, so just the one will have to do.
	//
	// rd = ref, in this case. It has already been calculated during the lighting function, so we're sacrificing a little readability
	// and reusing it. Correct me if I'm wrong, but I'm pretty sure the reflected vector is already normalized, so there's no need to
	// normalize it again.
	//
	// The last thing I'll mention - and it's something that can help you avoid a lot of grief when doing reflections - is the point
	// where you cast the ray from. In theory, it's the surface point. However, if you use that exact point, the first surface you'll
	// return a hit from is the surface itself. Therefore, you need to inch the ray away from the surface point enough to not return
	// a collision. Just over the stop-threshold will do, but I've moved it just a little further than that. This is old code, so I
	// can't remember why I chose 5 times that amount. Perhaps I was being paranoid, but it works.
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

    // Clamping the lit pixel, then put it on the screen.
	fragColor = vec4(clamp(sceneColor, 0.0, 1.0), 1.0);

}
