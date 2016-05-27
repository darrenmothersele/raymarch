cmake_minimum_required(VERSION 3.5)
project(raymarch)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

find_package(GLEW REQUIRED)

set(FREEIMAGE_FIND_REQUIRED, TRUE)
find_package(freeimage)
include_directories(SYSTEM ${FREEIMAGE_INCLUDE_DIRS})

find_package(PkgConfig REQUIRED)
pkg_search_module(GLFW REQUIRED glfw3)
include_directories(${GLFW_INCLUDE_DIRS})

find_package(GLM REQUIRED)

find_package(OpenGL REQUIRED)

include_directories(lib/embed-resource)
add_subdirectory(lib/embed-resource)

embed_resources(Shaders shaders/vertex.glsl shaders/frag.glsl shaders/frag2.glsl shaders/frag3.glsl shaders/frag4.glsl)

set(SOURCE_FILES main.cpp)
add_executable(raymarch ${SOURCE_FILES} ${Shaders})
target_link_libraries(raymarch
        ${FREEIMAGE_LIBRARIES}
        ${GLEW_LIBRARIES}
        ${GLFW_STATIC_LIBRARIES}
        ${OPENGL_gl_LIBRARY}
        )
