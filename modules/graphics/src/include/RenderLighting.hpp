//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include <array>
#include <cmath>
#include <vector>
#include "RenderInterface.hpp"
//=============================================================================
// Backend-agnostic per-vertex lighting (Gouraud). This is the single source of
// truth for CPU-rendered lighting and WebGL meshes whose colors are prepared by
// the native Web renderer. The model matrix is a column-major OpenGL modelview
// matrix (double[16]); vertices/normals are given in object space.
//=============================================================================
namespace RenderLighting {
//=============================================================================
struct PreparedLight
{
    point value;
    std::array<double, 3> color;
    bool isLocal = false;
};
//=============================================================================
struct PreparedLighting
{
    bool enabled = false;
    std::array<double, 12> pointTransform {};
    std::array<double, 9> normalTransform {};
    std::vector<PreparedLight> lights;
    std::array<double, 3> ambient {};
    double diffuseStrength = 0.;
    double specularStrength = 0.;
    double specularExponent = 0.;
    double specularColorReflectance = 0.;
};
//=============================================================================
inline double
lightingDot(const point& a, const point& b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}
//=============================================================================
inline point
lightingNormalize(point value)
{
    double norm = std::sqrt(lightingDot(value, value));
    if (!std::isfinite(norm) || norm <= 0) {
        return point(0, 0, 1);
    }
    value.x /= norm;
    value.y /= norm;
    value.z /= norm;
    return value;
}
//=============================================================================
inline point
lightingTransformVector(const double* matrix, const point& value)
{
    return point(matrix[0] * value.x + matrix[4] * value.y + matrix[8] * value.z,
        matrix[1] * value.x + matrix[5] * value.y + matrix[9] * value.z,
        matrix[2] * value.x + matrix[6] * value.y + matrix[10] * value.z);
}
//=============================================================================
inline point
lightingTransformDirection(const double* matrix, const point& value)
{
    point xAxis = lightingNormalize(point(matrix[0], matrix[1], matrix[2]));
    point yAxis = lightingNormalize(point(matrix[4], matrix[5], matrix[6]));
    point zAxis = lightingNormalize(point(matrix[8], matrix[9], matrix[10]));
    return point(xAxis.x * value.x + yAxis.x * value.y + zAxis.x * value.z,
        xAxis.y * value.x + yAxis.y * value.y + zAxis.y * value.z,
        xAxis.z * value.x + yAxis.z * value.y + zAxis.z * value.z);
}
//=============================================================================
inline point
lightingTransformPoint(const double* matrix, const point& value)
{
    return point(matrix[0] * value.x + matrix[4] * value.y + matrix[8] * value.z + matrix[12],
        matrix[1] * value.x + matrix[5] * value.y + matrix[9] * value.z + matrix[13],
        matrix[2] * value.x + matrix[6] * value.y + matrix[10] * value.z + matrix[14]);
}
//=============================================================================
inline point
lightingTransformNormal(const double* matrix, const point& value)
{
    double a00 = matrix[0];
    double a01 = matrix[4];
    double a02 = matrix[8];
    double a10 = matrix[1];
    double a11 = matrix[5];
    double a12 = matrix[9];
    double a20 = matrix[2];
    double a21 = matrix[6];
    double a22 = matrix[10];
    double c00 = a11 * a22 - a12 * a21;
    double c01 = a12 * a20 - a10 * a22;
    double c02 = a10 * a21 - a11 * a20;
    double c10 = a02 * a21 - a01 * a22;
    double c11 = a00 * a22 - a02 * a20;
    double c12 = a01 * a20 - a00 * a21;
    double c20 = a01 * a12 - a02 * a11;
    double c21 = a02 * a10 - a00 * a12;
    double c22 = a00 * a11 - a01 * a10;
    double determinant = a00 * c00 + a01 * c01 + a02 * c02;
    if (!std::isfinite(determinant) || std::abs(determinant) <= 1e-15) {
        return lightingTransformVector(matrix, value);
    }
    return point((c00 * value.x + c01 * value.y + c02 * value.z) / determinant,
        (c10 * value.x + c11 * value.y + c12 * value.z) / determinant,
        (c20 * value.x + c21 * value.y + c22 * value.z) / determinant);
}
//=============================================================================
inline PreparedLighting
prepareLighting(
    const double* model, const RenderLightingScene& scene, const RenderLightingMaterial& material)
{
    PreparedLighting prepared;
    prepared.enabled = scene.enabled && !scene.lights.empty();
    if (!prepared.enabled) {
        return prepared;
    }
    const int pointIndices[12] = { 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14 };
    for (size_t i = 0; i < prepared.pointTransform.size(); ++i) {
        prepared.pointTransform[i] = model[pointIndices[i]];
    }
    const point transformedX = lightingNormalize(point(model[0], model[1], model[2]));
    const point transformedY = lightingNormalize(point(model[4], model[5], model[6]));
    const point transformedZ = lightingNormalize(point(model[8], model[9], model[10]));
    const double a00 = model[0], a01 = model[4], a02 = model[8];
    const double a10 = model[1], a11 = model[5], a12 = model[9];
    const double a20 = model[2], a21 = model[6], a22 = model[10];
    const double c00 = a11 * a22 - a12 * a21;
    const double c01 = a12 * a20 - a10 * a22;
    const double c02 = a10 * a21 - a11 * a20;
    const double c10 = a02 * a21 - a01 * a22;
    const double c11 = a00 * a22 - a02 * a20;
    const double c12 = a01 * a20 - a00 * a21;
    const double c20 = a01 * a12 - a02 * a11;
    const double c21 = a02 * a10 - a00 * a12;
    const double c22 = a00 * a11 - a01 * a10;
    const double determinant = a00 * c00 + a01 * c01 + a02 * c02;
    if (std::isfinite(determinant) && std::abs(determinant) > 1e-15) {
        prepared.normalTransform = { c00 / determinant, c01 / determinant, c02 / determinant,
            c10 / determinant, c11 / determinant, c12 / determinant, c20 / determinant,
            c21 / determinant, c22 / determinant };
    } else {
        prepared.normalTransform = { a00, a01, a02, a10, a11, a12, a20, a21, a22 };
    }
    prepared.ambient = { material.ambientStrength * scene.ambientLightColor[0],
        material.ambientStrength * scene.ambientLightColor[1],
        material.ambientStrength * scene.ambientLightColor[2] };
    prepared.diffuseStrength = material.diffuseStrength;
    prepared.specularStrength = material.specularStrength;
    prepared.specularExponent = std::max(0., material.specularExponent);
    prepared.specularColorReflectance = material.specularColorReflectance;
    prepared.lights.reserve(scene.lights.size());
    for (const RenderLight& light : scene.lights) {
        point value(light.position[0], light.position[1], light.position[2]);
        if (light.isLocal) {
            value = lightingTransformPoint(model, value);
        } else {
            value = lightingNormalize(point(
                transformedX.x * value.x + transformedY.x * value.y + transformedZ.x * value.z,
                transformedX.y * value.x + transformedY.y * value.y + transformedZ.y * value.z,
                transformedX.z * value.x + transformedY.z * value.y + transformedZ.z * value.z));
        }
        prepared.lights.push_back(
            { value, { light.color[0], light.color[1], light.color[2] }, light.isLocal });
    }
    return prepared;
}
//=============================================================================
inline point
preparedTransformPoint(const PreparedLighting& prepared, const point& value)
{
    const auto& m = prepared.pointTransform;
    return point(m[0] * value.x + m[1] * value.y + m[2] * value.z + m[3],
        m[4] * value.x + m[5] * value.y + m[6] * value.z + m[7],
        m[8] * value.x + m[9] * value.y + m[10] * value.z + m[11]);
}
//=============================================================================
inline point
preparedTransformNormal(const PreparedLighting& prepared, const point& value)
{
    const auto& m = prepared.normalTransform;
    return point(m[0] * value.x + m[1] * value.y + m[2] * value.z,
        m[3] * value.x + m[4] * value.y + m[5] * value.z,
        m[6] * value.x + m[7] * value.y + m[8] * value.z);
}
//=============================================================================
// Ambient + per-light Lambert diffuse + Phong specular. Ambient is a single global term
// (per-light GL ambient is zero). The specular exponent is not clamped here, unlike the GL path
// (128).
//=============================================================================
inline RGBAColorData
computeLitColor(const RGBAColorData& color, const point& normal, const point& vertex,
    const PreparedLighting& prepared)
{
    if (!prepared.enabled) {
        return color;
    }
    point vertexEye = preparedTransformPoint(prepared, vertex);
    point n = lightingNormalize(preparedTransformNormal(prepared, normal));
    point viewDirection = lightingNormalize(point(-vertexEye.x, -vertexEye.y, -vertexEye.z));
    // Two-sided lighting: a normal pointing away from the viewer is flipped so back faces are lit
    // instead of staying at the ambient term.
    if (lightingDot(n, viewDirection) < 0.) {
        n = point(-n.x, -n.y, -n.z);
    }
    double r = prepared.ambient[0];
    double g = prepared.ambient[1];
    double b = prepared.ambient[2];
    double specularR = 0.;
    double specularG = 0.;
    double specularB = 0.;
    for (const PreparedLight& light : prepared.lights) {
        point direction = light.isLocal
            ? lightingNormalize(point(light.value.x - vertexEye.x, light.value.y - vertexEye.y,
                  light.value.z - vertexEye.z))
            : light.value;
        double diffuse = std::max(0., lightingDot(n, direction));
        r += prepared.diffuseStrength * diffuse * light.color[0];
        g += prepared.diffuseStrength * diffuse * light.color[1];
        b += prepared.diffuseStrength * diffuse * light.color[2];
        if (diffuse > 0. && prepared.specularStrength > 0.) {
            // Phong reflection (light reflected about the normal, compared to the view direction).
            // The half-vector form gives a broader, brighter highlight for the same exponent.
            point reflected = lightingNormalize(point(2. * diffuse * n.x - direction.x,
                2. * diffuse * n.y - direction.y, 2. * diffuse * n.z - direction.z));
            double specular = std::pow(std::max(0., lightingDot(reflected, viewDirection)),
                                  prepared.specularExponent)
                * prepared.specularStrength * prepared.specularColorReflectance;
            specularR += specular * light.color[0];
            specularG += specular * light.color[1];
            specularB += specular * light.color[2];
        }
    }
    return RGBAColorData(std::min(1., color.r * r + specularR),
        std::min(1., color.g * g + specularG), std::min(1., color.b * b + specularB), color.a);
}
//=============================================================================
inline RGBAColorData
computeLitColor(const RGBAColorData& color, const point& normal, const point& vertex,
    const double* model, const RenderLightingScene& scene, const RenderLightingMaterial& material)
{
    return computeLitColor(color, normal, vertex, prepareLighting(model, scene, material));
}
//=============================================================================
} // namespace RenderLighting
//=============================================================================
