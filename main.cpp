#include <iostream>
#include <vector>
#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <FreeImage.h>
#include "Resource.h"

#define WIDTH 600
#define HEIGHT 420

using namespace std;
using namespace glm;

int main() {

    if (!glfwInit())
        exit(EXIT_FAILURE);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);

    // Full screen
//    auto monitor = glfwGetPrimaryMonitor();
//    const GLFWvidmode* mode = glfwGetVideoMode(monitor);
//    glfwWindowHint(GLFW_RED_BITS, mode->redBits);
//    glfwWindowHint(GLFW_GREEN_BITS, mode->greenBits);
//    glfwWindowHint(GLFW_BLUE_BITS, mode->blueBits);
//    glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
//    GLFWwindow* window = glfwCreateWindow(mode->width, mode->height, "Raymarching", monitor, nullptr);

    GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "Raymarching", nullptr, nullptr);

    if (!window)
        exit(EXIT_FAILURE);

    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);

    glewExperimental = GL_TRUE;
    if (glewInit() != GLEW_OK)
        exit(EXIT_FAILURE);

    int screenWidth, screenHeight;
    glfwGetFramebufferSize(window, &screenWidth, &screenHeight);
    glViewport(0, 0, screenWidth, screenHeight);

    glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


    Resource vert = LOAD_RESOURCE(vertex_glsl);
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    const char *vertexShaderCode = string(vert.data(), vert.size()).c_str();
    glShaderSource(vertexShader, 1, &vertexShaderCode, NULL);
    glCompileShader(vertexShader);
    GLint success;
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        GLchar infoLog[512];
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        cout << "Vertex Shader Error:" << endl << infoLog << endl;
        exit(EXIT_FAILURE);
    }

    Resource frag = LOAD_RESOURCE(frag2_glsl);
    GLuint fragShader = glCreateShader(GL_FRAGMENT_SHADER);
    const char *fragShaderCode = string(frag.data(), frag.size()).c_str();
    glShaderSource(fragShader, 1, &fragShaderCode, NULL);
    glCompileShader(fragShader);
    // GLint success;
    glGetShaderiv(fragShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        GLchar infoLog[512];
        glGetShaderInfoLog(fragShader, 512, NULL, infoLog);
        cout << "Fragment Shader Error:" << endl << infoLog << endl;
        exit(EXIT_FAILURE);
    }

    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragShader);
    glLinkProgram(shaderProgram);
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if (!success)
    {
        GLchar infoLog[512];
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        std::cout << "Shader link error:" << endl << infoLog << std::endl;
        exit(EXIT_FAILURE);
    }

    glDeleteShader(vertexShader);
    glDeleteShader(fragShader);

    glUseProgram(shaderProgram);

    auto resolutionLocation = glGetUniformLocation(shaderProgram, "resolution");
    glUniform2f(resolutionLocation, screenWidth, screenHeight);

    auto mouseLocation = glGetUniformLocation(shaderProgram, "mouse");
    glUniform2f(mouseLocation, 0, 0);

    float mx = std::max<float>(screenWidth, screenHeight);
    auto xDim = screenWidth/mx;
    auto yDim = screenHeight/mx;
    auto screenRatioLocation = glGetUniformLocation(shaderProgram, "screenRatio");
    glUniform2f(screenRatioLocation, xDim, yDim);

    auto timeLocation = glGetUniformLocation(shaderProgram, "time");
    glUniform1f(timeLocation, 0.0);

    GLuint vao, vbo, ebo;

    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);
    glGenBuffers(1, &ebo);

    vector<vec2> vertices;
    vector<GLuint> indices;

    unsigned int offset = (unsigned int)vertices.size();

    vertices.push_back({-1.0f, -1.0f});
    vertices.push_back({1.0f, -1.0f});
    vertices.push_back({-1.0f, 1.0f});
    vertices.push_back({1.0f, 1.0f});

    indices.push_back(offset + 0);
    indices.push_back(offset + 1);
    indices.push_back(offset + 2);
    indices.push_back(offset + 2);
    indices.push_back(offset + 1);
    indices.push_back(offset + 3);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(vec2), &vertices[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.size() * sizeof(GLuint), &indices[0], GL_STATIC_DRAW);
    // Vertex positions
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(vec2), (GLvoid*)0);

    glBindVertexArray(0);

//    BYTE* pixels = new BYTE[3 * screenWidth * screenHeight];
//    FIBITMAP* image;
//    int i = 0;
    double xPos, yPos;

    while (!glfwWindowShouldClose(window))
    {
        glUniform1f(timeLocation, (GLfloat)glfwGetTime());

        glfwGetCursorPos(window, &xPos, &yPos);
        glUniform2f(mouseLocation, (GLfloat)xPos, (GLfloat)yPos);

        glBindVertexArray(vao);
        glDrawElements(GL_TRIANGLES, (GLsizei)indices.size(), GL_UNSIGNED_INT, 0);
        glBindVertexArray(0);

//        glReadBuffer(GL_BACK);
//        glReadPixels(0, 0, screenWidth, screenHeight, GL_RGB, GL_UNSIGNED_BYTE, pixels);
//        image = FreeImage_ConvertFromRawBits(pixels, screenWidth, screenHeight, 3 * screenWidth, 24, 0xFF0000, 0x00FF00, 0x0000FF, false);
//        FreeImage_Save(FIF_BMP, image, string("output_" + to_string(i++) + ".bmp").c_str(), 0);

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwTerminate();
    exit(EXIT_SUCCESS);
}
