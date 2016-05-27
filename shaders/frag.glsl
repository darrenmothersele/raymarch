#version 330 core

uniform float time;
uniform vec2 resolution;
uniform vec4 mouse;

out vec4 fragColor;

//in vec2 surfacePosition;

// Raymarching Template
// Source - Raymarching.com
// Author - Gary "Shane" Warne
// eMail - mail@Raymarching.com, mail@Labyrinth.com
// Last update: 28th Aug, 2014
//
// Don't you just love it when coders reuse the same algorthms that have been around of years, rearrange them a little, change the variable names,
// then attach one those threatening, copyright preambles to their work. I've seen copyright notices tagged onto ten-line plasma demos. Hilarious.
// Some coders definitely need to lay off the fairy dust, that's for sure.
//
// All of the code on this page has been written a gazillion times by a zillion different people. There is nothing new here. I guess that's why
// I've called it a template - because it's a relative cliche of a basic WebGL raymarching pipeline. Screen coordinates, camera setup, ray setup, raymarch,
// lighting, screen render, and done. Use it for whatever you purpose you like.
//
// Anyway, I'm genuinely grateful for all the code various people have made public over at "Shadertoy.com," which is run by that RGBA guy, Inigo Quilez,
// aka IQ. "Shader Toy" is an apt description for that site, because as a person who loves graphic-related snippets, I'm like a kid in a toy store
// over there. I guess you needed to have been on the net 15, or even 10, years ago trying to hunt down demo effects code to really appreciate what's
// being offered at "ShaderToy.com." Fantastic stuff.

#define PI 3.1415926535898 // That was from memory, so if things start flying off the screen...

// Epsilon value. I prefer to arrange for it to be in the order of a pixel, but it's definitely worth changing to a value that works for you.
// Larger numbers can give rounder looking corners, and can even take away a little aliasing, sometimes. However, larger numbers are obviously less
// accurate, and can make the edges appear lighter, darker, etc, than they should... It all depends on what you're looking to acheive.
const float eps = 0.005;

// I could spend all day just talking about the following three settings alone, but I'll keep it brief. Obviously, more raymarching iterations,
// coupled with a smaller step-scale and stop-threshold, will give you a more accurate rendering, but at the expense of frame rate. Finding the right
// balance is almost an artform. In this particular example, not much is being done, so I've treated myself. Normally, I'd probably have to make do
// with 64 iterations, a step-scale of 0.75, and a higher stop-threshold, which has a tendency to smoothen off surface features when you don't want
// it to. Computers either need to get faster, or I need to become a better coder...  but since the latter's not going to happen, I'm counting on SGI
// and 3dfx to make it happen. I've been away for a while. They're still the best graphics card makers around, right?
const int maxIterations = 128;
const float stepScale = 0.5;
const float stopThreshold = 0.005; // I'm not quite sure why, but thresholds in the order of a pixel seem to work better for me... most times.

// The sphere equation. Next to the plane equation, arguably the most boring isosuface equation around. Good for learning purposes though. We're
// looking for points in 3d space where the isosurface equation below is close to zero. The easiest way to see whether the function below works is
// to picture a 1 unit radius sphere centered on the origin, which would mean (length(p) - 1 = 0), which expanded, rearranged, etc, gives:
// (x*x + y*y + z*z = 1).
//
// Therefore, you'd expect surface points (1,0,0), (-1,0,0), (0,1,0), (0,-1, 0), etc, to all return hits, which they do. Sure, I might be stating the
// obvious to some, but the entire concept of raymarching is based on evaluating isosurface functions, so sometimes it helps to plug in some real-world
// values just to get an initial feel for it.
float sphere(in vec3 p, in vec3 centerPos, float radius) {

	return length(p-centerPos) - radius;
}

// WTF has this clown done here? Good question. A smooth spherical surface doesn't really do raymarching enough justice, so I've added some cheap bumps.
// A sphere is easy to raytrace, and raymarch for that matter. The function below can be raytraced, but is only "easy" to do in a raymarching setting.
// The term "cheap" is relative, of course. A sinusoidal function tends to be a CPU killer, whereas the GPU seems to take it in its stride.
//
// Anyway, A*sin(f*p.x)*sin(f*p.y)*sin(f*p.z) - "f" for frequency and "A" for amplitude - is just one of the ways to put bumps on a sphere. I've also
// added an extra term with half the amplitude and twice the frequency for a little more detail. Yes, smooth noise, Voronoi, etc, would be more
// interesting, but this is just a simple template. The "time" term moves the bumps around a little, which gives the object a bit of movement and gives
// the specular lighting a better chance to shine... See what I did there? I also could have gone with a corny "in the spotlight" analogy.
//
// By the way, it's actually possible to to perform some layering, combined with 3D rotations, to make this "much" nicer. In fact, possibly better than
// cheap noise, but we'll keep things simple, for now.
float sinusoidBumps(in vec3 p){

    return sin(p.x*16.+time*0.57)*cos(p.y*16.+time*2.17)*sin(p.z*16.-time*1.31) + 0.5*sin(p.x*32.+time*0.07)*cos(p.y*32.+time*2.11)*sin(p.z*32.-time*1.23);
}

// This is the raymarched scene that gets called multiple times from the raymarching procedure, which means adding detail can slow things right down.
// In this case, though, we're not doing that much. Smallish bumps have been added to a sphere, of radius 1, that is placed at position (0.0, 0.0, 2.0).
// A modern GPU shouldn't have too many problems doing that.
float scene(in vec3 p) {

	return sphere(p, vec3(0., 0. , 2.), 1.) + 0.04*sinusoidBumps(p);
}

// A clever way to obtain the surface normal without the need to perform difficult, and often expensive, differential calculations, etc.
// Use the surface point (p) and epsilon value (eps) to obtain the gradient along each of the individual axes (f(p.x+eps)-f(p.x-eps), etc).
// That should give you a scaled representation of (df/dx, df/dy, df/dz), which can simply be normalized to get the unit normal.
//
// I guess it's the 3D equivalent of obtaining the gradient of a texel in a 2D texture for bump mapping. Back in the days, when no one would tell
// you anything, and you had to figure it out for yourself. I prefer to use "f(p.x+eps)-f(p.x)" to save on cycles, but it's not quite as accurate.
vec3 getNormal(in vec3 p) {


	// 6-tap normalization. Probably the most accurate, but a bit of a cycle waster.
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
    // If speed is an issue, here's a slightly-less-accurate, 4-tap version. If fact, visually speaking, it's virtually the same, so on a
    // lot of occasions, this is the one I'll use. However, if speed is really an issue, you could take away the "normalization" step, then
    // divide by "eps," but I'll usually avoid doing that.
    float ref = scene(p);
	return normalize(vec3(
		scene(vec3(p.x+eps,p.y,p.z))-ref,
		scene(vec3(p.x,p.y+eps,p.z))-ref,
		scene(vec3(p.x,p.y,p.z+eps))-ref
	));
	*/

	/*
	// The tetrahedral version, which does involve fewer calculations, but doesn't seem as accurate on some surfaces... I could be wrong,
	// but that's the impression I get.
	vec2 e = vec2(-0.5*eps,0.5*eps);
	return normalize(e.yxx*scene(p+e.yxx)+e.xxy*scene(p+e.xxy)+e.xyx*scene(p+e.xyx)+e.yyy*scene(p+e.yyy));
	*/
}

// I often wonder who first came up with the idea to iteratively sample an object's isosurface description along a ray to get a hit point. Very clever,
// and elegant too. Don't make the mistake I made and try to overthink it. The idea really is pretty simple, but is kind of difficult to put into words.
//
// In essense, a point is inched incrementally along a ray through the scene and tested against a distance-field equation, which gives the distance to the
// closest surface point. When a point along the ray is within a threshold close enough to a surface, the surface point is returned, after which it's passed
// down the pipeline to be lit and rendered to the screen.

// Take a point along the ray and plug it into the scene equation. That equation will give the distance from the ray-point to the closest surface
// point in the scene. Obviously, if the distance returned is close to zero, then the ray point is at the surface object. If it's not, then you have to
// move further along the ray, and try again. Fair enough, but the question is, how far do you move along the ray? "A diagram is worth a thousand lines of code,"
// as they say, so one of those "spheres along the ray" visual aids would be helpful right about now. However, take my word that you can, at least, move
// the distance returned from the scene equation without overshooting your hit point. The reason being that you're guaranteed there's not a surface point
// along the ray that is closer... Even I'm starting to drift off at this point.

// If you're not familiar with the concept, then the above "explanation" might not be that helpful, so look the up process on the net. It's a lot easier
// to understand with the help of visual diagrams. For what it's worth, I think the algorithm itself explains the process better than a human can.

// Raymarching, or ray marching, if you prefer. The algorthmic version of the expression, "Am I there yet?" Between you and me, I know the guy who owns
// "Raymarching.com," and he has all the common sense of a rabid squirrel on methamphetamines, so if he can get his head around the concept, you shouldn't
// have any trouble.
float rayMarching( vec3 origin, vec3 dir, float start, float end ) {

	float sceneDist = 1e4;
	float rayDepth = start; // Ray depth. "start" is usually zero, but for various reasons, you may wish to start the ray further away from the origin.
	for ( int i = 0; i < maxIterations; i++ ) {

		sceneDist = scene( origin + dir * rayDepth ); // Distance from the point along the ray to the nearest surface point in the scene.

        // Irregularities between browsers have forced me to use this logic. I noticed that Firefox was interpreting two "if" statements inside a loop
        // differently to Chrome, and... 20 years on the web, so I guess I should be used to this kind of thing.
        //
        // Anyway, belive it or not, the stop threshold is one of the most important values in your entire application. Smaller numbers are more
        // accurate, but can slow your program down - drastically, at times. Larger numbers can speed things up, but at the cost of aesthetics.
        // Swapping a number, like "0.001," for something larger, like "0.01," can make a huge difference in framerate.
		if (( sceneDist < stopThreshold ) || (rayDepth >= end)) {

		    // (rayDepth >= end) - We haven't used up all our iterations, but the ray has reached the end of the known universe... or more than
		    // likely, just the far-clipping-plane. Either way, it's time to return the maximum distance.

		    // (sceneDist < stopThreshold) - The distance is pretty close to zero, which means the point on the ray has effectively come into contact
		    // with the surface. Therefore, we can return the distance, which can be used to calculate the surface point.


			// I'd rather neatly return the value above. Chrome and IE are OK with it. Firefox doesn't like it, etc... I don't know, or care,
			// who's right and who's wrong, but I would have thought that enabling users to execute a simple "for" loop without worring about what
			// will work, and what will not, would be a priority amongst the various parties involved. Anyway, that's my ramble for the day. :)
			break;
		}
		// We haven't hit anything, so increase the depth by a scaled factor of the minimum scene distance. It'd take too long to explain why
		// we'd want to increase the ray depth by a smaller portion of the minimum distance, but it can help, believe it or not.
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

	// We've used up our maximum iterations. Damn, just a few more, and maybe we could have hit something, or maybe there was nothing to hit.
	// Either way, return the maximum distance, which is usually the far-clipping-plane, and be done with it.
	return rayDepth;
}


void main(void) {


    // Setting up our screen coordinates: gl_FragCoord.xy represents our screen pixels (or screen coordinates, if you prefer), which range
    // from [0 to resolution.x] along the x-axis, and [0 to resolution.y] along the y-axis... If you wish to get technical, I hear they
    // range from [0.5 to resolution.x-0.5] and [0.5 to resolution.y-0.5] inclusive, but you know what I mean. :)
    //
    // However, to make calculations easier later on, we'd like have screen coordinates that center on (0.0, 0.0) and also fall within a range
    // of about [-1.0 to 1.0] along each of the axes. In order for the image to not appear distorted (squashed), we also factor in the screen's
    // aspect ratio ( screen_width/screen_height ), which in this case is (resolution.x/resolution.y).
    //
    // By the way, you don't actually have to do this. However, having the center of the screen at (0.0, 0.0) just makes life easier...
    // Besides, I'm a follower, and that's what all the cool kids are doing these days.

    vec2 aspect = vec2(resolution.x/resolution.y, 1.0);
	vec2 screenCoords = (2.0*gl_FragCoord.xy/resolution.xy - 1.0)*aspect;


	// The camPos vector is the location of our vantage point, eye point, camera position, or whatever else people wish to call it. The lookAt vector
	// effectively represents the center of the screen we're projecting the rays from the scene onto. In essence, camPos is the vector position we're
	// looking from, and lookAt is the position of the screen we're looking at, or through, if you prefer.

	vec3 lookAt = vec3(0.,0.,0.);  // This is the point you look towards, or at.
	vec3 camPos = vec3(0., 0., -1.); // This is the point you look from, or camera you look at the scene through. Whichever way you wish to look at it.

	// The following uses the above and some pretty standard vector math to set up the screen that we're going to project onto. Use the lookAt and camPos
	// vectors to contruct a forward vector. Use that vector to construct a vector perpendicular to it, namely the "right" vector, then cross product
	// the forward vector with the "right" vector to produce the "up" vector. All three of these are used to construct the unit direction ray, namely,
	// "rd" that will be cast off into the scene from our vantage point (aka, eye, camera position, etc) , which we've called camPos.

    // Camera setup.
    vec3 forward = normalize(lookAt-camPos); // Forward vector.
    vec3 right = normalize(vec3(forward.z, 0., -forward.x )); // Right vector... or is it left? Either way, so long as the correct-facing up-vector is produced.
    vec3 up = normalize(cross(forward,right)); // Cross product the two vectors above to get the up vector.

    // FOV - Field of view. Make it bigger, and the screen covers a larger area, which means more of the scene can be seen. This, in turn, means that our
    // objects will appear smaller.
    float FOV = 0.25;

    // ro - Ray origin. Every ray starts from this point, then is cast in the rd direction.
    vec3 ro = camPos;
    // rd - Ray direction. This is our one-unit-long direction ray.
    vec3 rd = normalize(forward + FOV*screenCoords.x*right + FOV*screenCoords.y*up);


	// Yeah, I agree, the above is confusing. I wrote this shit, and if I read it for the first time, it'd confuse me too... but don't worry, I once knew
	// this guy, who knew another guy, who told him that he once, back in 1979, raymarched 3000 levels of Quake 28 simultaneously on ten Casio calculators.
	// Each were hooked up in parallel using jump cables predipped in battery acid. Anyway, apparently, he said the above was a stock-standard raymarching setup.
	//
	// Either way, if you don't quite understand yet, just know that it works, and figure out "why" it works by reading some articles. In fact, I'll probably
	// write one myself at some stage.


    // OK, the screen setup, camera setup, etc, explanations were about as interesting as watching paint dry, but the cool thing is, you can pretty much forget
    // a good portion of what is written above. The only things we're interested in are the following:
    //
    // camPos - That'll allow us to move about the scene. However, movement instructions need to be made before the camera setup.
    // lookAt - We'll sometimes have to change this to suit the camera movements. Again, movement instructions need to be made before the camera setup.
    // ro - We initially set the ray origin to the camera position, but it can change to an object's surfact position when doing reflections, or checking for shadows.
    // rd - There are two different ways to raymarch. There's the discrete, incremental approach, and sphere-tracing, which tends to be more effective.	More about that later.



	// The screen's background color.
    vec3 bgcolor = vec3(1.,0.97,0.92)*0.15;
    // Oh great, another obfuscated mess to decipher. I could have left the background alone, but I wanted to give it a bit of a fake backlight.
    // Then, I wanted to shape it a bit, etc. It's not essential, but it looks a little nicer.
    float bgshade = (1.0-length(vec2(screenCoords.x/aspect.x, screenCoords.y+0.5) )*0.8);
	bgcolor *= bgshade; //Shade the background a little.


	// Ray marching.
	// Set the near and far clipping planes, then supply them - along with the ray origin and ray direction vectors - to the rayMarching routine.
	// If a surface point in the scene is hit, a distance value (dist) less than the maximum value (clipFar) will be returned.
	const float clipNear = 0.0;
	const float clipFar = 4.0;
	float dist = rayMarching(ro, rd, clipNear, clipFar ); // See the function itself for an explanation.
	if ( dist >= clipFar ) {
	    // I prefer to do it this way in order to avoid an if-statement below, but I honestly couldn't say whether it's more
	    // efficient. It feels like it would be. Does that count? :)
	    fragColor = vec4(bgcolor, 1.0);
	    return;
		//discard; // If you want to return without rendering anything, I think.
	}

	// sp - Surface position. If we've made it this far, we've hit something. Use the "dist" value from above to obtain the surface postion, "sp,"
	// which can be passed down the pipeline for lighting.
	vec3 sp = ro + rd*dist;

	// We can use the surface position to calculate the surface normal using a bit of vector math. I remember having to give long, drawn-out,
	// sleep-inducing talks at uni on implicit surface geometry (or something like that) that involved normals on 3D surfaces and such.
	// I barely remember the content, but I definitely remember there was always this hot chick in the room with a gigantic set of silicons who
	// looked entirely out of place amongst all the nerds... and this was back in the days when those things weren't as common... Um, I forgot
	// where I was going with this.
	//
	// Anyway, check out the function itself. It's a standard, but pretty clever way to get a surface normal on difficult-to-differentiate surfaces.
	vec3 surfNormal = getNormal(sp);

	// Lighting. Just the one light. It needs to have a position, a direction and a color. Obviously, it should be positioned away from the
	// object's surface. The direction vector is the normalized vector running from the light position to the object's surface point that we're
	// going to illuminate. You can choose any light color you want, but it's probably best to choose a color that works best with the colors
	// in the scene. I've gone for a warmish white.

	// lp - Light position. I've arranged for it to move in a bit of a circle about the xy-plane a couple of units away from the spherical object.
	vec3 lp = vec3(1.5*sin(time*0.5), 0.75+0.25*cos(time*0.5), -1.0);
	// ld - Light direction. The point light direction goes from the light's position to the surface point we've hit on the sphere. I haven't
	// normalized it yet, because I'd like to take the length first, but it will be.
	vec3 ld = lp-sp;
	// lcolor - Light color. I have it in my head that light globes give off this color, but I swear I must have pulled that information right
	// out of my a... Choose any color you want.
	vec3 lcolor = vec3(1.,0.97,0.92);


	// Light falloff (attenuation), which depends on how far the surface point is from the light. Most of the time, I guess the falloff rate should be
	// mixtures of inverse distance powers, but in real life, it's far more complicated than that. Either way, most of the time you should simply
	// choose whatever makes the lighting look a little prettier. For instance, if things look too dark, I might decide to make the falloff drop off
	// linearly, without any other terms. In this case, the light is falling off with the square of the distance, and no other terms.

	float len = length( ld ); // Distance from the light to the surface point.
	ld /= len; // Normalizing the light-to-surface, aka light-direction, vector.
	float lightAtten = min( 1.0 / ( 0.25*len*len ), 1.0 ); // Keeps things between 0 and 1.

	// The unit-length, reflected vector. Angle of incidence equals angle of reflection, if you remember rudimentary highschool physics, or math.
	// Anyway, the incident (incoming... for want of a better description) vector is the vector representing our line of sight from the light position
	// to the point on the suface of the object we've just hit. We get the reflected vector on the surface of the object by doing a quick calculation
	// between the incident vector and the surface normal. The reflect function is ( ref=incidentNorm-2.0*dot(incidentNorm, surfNormal)*surfNormal ),
	// or something to that effect. Either way, there's a function for it, which is used below.
	//
	// The reflected vector is useful, because we can use it to calculate the specular reflection component. For all intents and purposes, specular light
	// is the light gathered in the mirror direction. I like it, because it looks pretty, and I like pretty things. One of the most common mistakes made
	// with regard to specular light calculations is getting the vector directions wrong, and I've made the mistake more than a few times. So, if you
	// notice I've got the signs wrong, or anything, feel free to let me know.
	vec3 ref = reflect(-ld, surfNormal);


    // Start with black. If we had global ambient lighting, then I guess we could add it here, or later. It's a preference thing.
	vec3 sceneColor = vec3(0.0);

	// The spherical object's color. My favorite color is black, but I don't think that will work, so I've gone with something greenish.
	vec3 objColor = vec3(1.0, 0.6, 0.8);
	// Just some really lame, fake shading/coloring for the object. You can comment the two lines out with no consequence.
	float bumps =  sinusoidBumps(sp);
    objColor = clamp(objColor*0.8-vec3(0.4, 0.2, 0.1)*bumps, 0.0, 1.0);

	float ambient = .1; //The object's ambient property. You can also have a global and light ambient property, but we'll try to keep things simple.
	float specularPower = 16.0; // The power of the specularity. Higher numbers can give the object a harder, shinier look.
	float diffuse = max( 0.0, dot(surfNormal, ld) ); //The object's diffuse value, which depends on the angle that the light hits the object.
	//The object's specular value, which depends on the angle that the reflected light hits the object, and the viewing angle... kind of.
	float specular = max( 0.0, dot( ref, normalize(camPos-sp)) );
	specular = pow(specular, specularPower); // Ramping up the specular value to the specular power for a bit of shininess.

	// Bringing all the lighting components together to color the screen pixel. By the way, this is a very simplified version of Phong lighting.
	// It's "kind of" correct, and will suffice for this example. After all, a lot of lighting is fake anyway.
	sceneColor += (objColor*(diffuse*0.8+ambient)+specular*0.5)*lcolor*lightAtten;

    // Clamping the lit pixel between black and while, then putting it on the screen. We're done. Hooray!
	fragColor = vec4(clamp(sceneColor, 0.0, 1.0), 1.0);

}
