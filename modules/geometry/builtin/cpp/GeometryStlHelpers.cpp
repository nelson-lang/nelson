//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GeometryStlHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <cwctype>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <limits>
#include <map>
#include <sstream>
//=============================================================================
namespace Nelson::GeometryGateway::Stl {
//=============================================================================
namespace {
    //=============================================================================
    struct Facet
    {
        std::array<std::array<double, 3>, 3> vertices;
        uint16 attribute = 0;
        double solidId = 1;
    };
    //=============================================================================
    std::wstring
    lowerWide(std::wstring value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(towlower(c)); });
        return value;
    }
    //=============================================================================
    std::string
    lowerAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    //=============================================================================
    void
    checkExtension(const std::wstring& filename)
    {
        std::wstring ext = lowerWide(std::filesystem::path(filename).extension().wstring());
        if (ext != L".stl") {
            Error(L"Nelson:stl:invalidFileName", L"File name must end with .stl or .STL.");
        }
    }
    //=============================================================================
    std::vector<unsigned char>
    readBytes(const std::wstring& filename)
    {
        std::ifstream stream(std::filesystem::path(filename), std::ios::binary);
        if (!stream) {
            Error(L"Nelson:stl:cannotOpenFile", L"Unable to open file: %s", filename);
        }
        return std::vector<unsigned char>(
            std::istreambuf_iterator<char>(stream), std::istreambuf_iterator<char>());
    }
    //=============================================================================
    uint32
    readUInt32(const unsigned char* data)
    {
        return static_cast<uint32>(data[0]) | (static_cast<uint32>(data[1]) << 8)
            | (static_cast<uint32>(data[2]) << 16) | (static_cast<uint32>(data[3]) << 24);
    }
    //=============================================================================
    uint16
    readUInt16(const unsigned char* data)
    {
        return static_cast<uint16>(data[0] | (data[1] << 8));
    }
    //=============================================================================
    float
    readFloat32(const unsigned char* data)
    {
        uint32 raw = readUInt32(data);
        float value;
        std::memcpy(&value, &raw, sizeof(float));
        return value;
    }
    //=============================================================================
    void
    appendUInt32(std::ofstream& stream, uint32 value)
    {
        unsigned char bytes[4] = { static_cast<unsigned char>(value & 0xff),
            static_cast<unsigned char>((value >> 8) & 0xff),
            static_cast<unsigned char>((value >> 16) & 0xff),
            static_cast<unsigned char>((value >> 24) & 0xff) };
        stream.write(reinterpret_cast<const char*>(bytes), 4);
    }
    //=============================================================================
    void
    appendUInt16(std::ofstream& stream, uint16 value)
    {
        unsigned char bytes[2] = { static_cast<unsigned char>(value & 0xff),
            static_cast<unsigned char>((value >> 8) & 0xff) };
        stream.write(reinterpret_cast<const char*>(bytes), 2);
    }
    //=============================================================================
    void
    appendFloat32(std::ofstream& stream, double value)
    {
        float narrowed = static_cast<float>(value);
        uint32 raw;
        std::memcpy(&raw, &narrowed, sizeof(float));
        appendUInt32(stream, raw);
    }
    //=============================================================================
    bool
    isBinaryStl(const std::vector<unsigned char>& bytes)
    {
        if (bytes.size() < 84) {
            return false;
        }
        uint32 count = readUInt32(bytes.data() + 80);
        uint64 expected = 84ull + 50ull * static_cast<uint64>(count);
        return expected == bytes.size();
    }
    //=============================================================================
    bool
    startsWithSolid(const std::vector<unsigned char>& bytes)
    {
        size_t index = 0;
        while (index < bytes.size() && std::isspace(bytes[index])) {
            ++index;
        }
        const std::string marker = "solid";
        if (bytes.size() - index < marker.size()) {
            return false;
        }
        for (size_t k = 0; k < marker.size(); ++k) {
            if (static_cast<char>(std::tolower(bytes[index + k])) != marker[k]) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    isDegenerate(const std::array<std::array<double, 3>, 3>& vertices)
    {
        std::array<double, 3> a = { vertices[1][0] - vertices[0][0],
            vertices[1][1] - vertices[0][1], vertices[1][2] - vertices[0][2] };
        std::array<double, 3> b = { vertices[2][0] - vertices[0][0],
            vertices[2][1] - vertices[0][1], vertices[2][2] - vertices[0][2] };
        std::array<double, 3> c
            = { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
        return c[0] == 0 && c[1] == 0 && c[2] == 0;
    }
    //=============================================================================
    std::array<double, 3>
    normalFor(const std::array<std::array<double, 3>, 3>& vertices)
    {
        std::array<double, 3> a = { vertices[1][0] - vertices[0][0],
            vertices[1][1] - vertices[0][1], vertices[1][2] - vertices[0][2] };
        std::array<double, 3> b = { vertices[2][0] - vertices[0][0],
            vertices[2][1] - vertices[0][1], vertices[2][2] - vertices[0][2] };
        std::array<double, 3> n
            = { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
        double scale = std::sqrt(n[0] * n[0] + n[1] * n[1] + n[2] * n[2]);
        if (scale > 0) {
            n[0] /= scale;
            n[1] /= scale;
            n[2] /= scale;
        }
        return n;
    }
    //=============================================================================
    void
    appendFacet(
        Mesh& mesh, std::map<std::array<double, 3>, indexType>& existing, const Facet& facet)
    {
        if (isDegenerate(facet.vertices)) {
            return;
        }
        std::array<indexType, 3> face;
        for (size_t k = 0; k < 3; ++k) {
            auto it = existing.find(facet.vertices[k]);
            if (it == existing.end()) {
                mesh.points.push_back(facet.vertices[k]);
                face[k] = static_cast<indexType>(mesh.points.size());
                existing[facet.vertices[k]] = face[k];
            } else {
                face[k] = it->second;
            }
        }
        mesh.connectivity.push_back(face);
        mesh.attributes.push_back(facet.attribute);
        mesh.solidIds.push_back(facet.solidId);
    }
    //=============================================================================
    Mesh
    readBinary(const std::vector<unsigned char>& bytes)
    {
        Mesh mesh;
        mesh.binary = true;
        std::map<std::array<double, 3>, indexType> existing;
        uint32 count = readUInt32(bytes.data() + 80);
        size_t offset = 84;
        for (uint32 k = 0; k < count; ++k) {
            Facet facet;
            const unsigned char* record = bytes.data() + offset;
            for (size_t vertex = 0; vertex < 3; ++vertex) {
                size_t base = 12 + vertex * 12;
                facet.vertices[vertex] = { readFloat32(record + base),
                    readFloat32(record + base + 4), readFloat32(record + base + 8) };
            }
            facet.attribute = readUInt16(record + 48);
            facet.solidId = 1;
            appendFacet(mesh, existing, facet);
            offset += 50;
        }
        return mesh;
    }
    //=============================================================================
    bool
    readVertex(std::istringstream& stream, std::array<double, 3>& vertex)
    {
        std::string token;
        while (stream >> token) {
            if (lowerAscii(token) == "vertex") {
                return static_cast<bool>(stream >> vertex[0] >> vertex[1] >> vertex[2]);
            }
        }
        return false;
    }
    //=============================================================================
    Mesh
    readText(const std::vector<unsigned char>& bytes)
    {
        Mesh mesh;
        mesh.binary = false;
        std::map<std::array<double, 3>, indexType> existing;
        std::string text(bytes.begin(), bytes.end());
        std::istringstream stream(text);
        std::string token;
        double currentSolid = 0;
        while (stream >> token) {
            token = lowerAscii(token);
            if (token == "solid") {
                currentSolid += 1;
                std::string rest;
                std::getline(stream, rest);
            } else if (token == "facet") {
                Facet facet;
                facet.solidId = currentSolid > 0 ? currentSolid : 1;
                std::string ignored;
                stream >> ignored >> ignored >> ignored >> ignored;
                if (!readVertex(stream, facet.vertices[0]) || !readVertex(stream, facet.vertices[1])
                    || !readVertex(stream, facet.vertices[2])) {
                    Error(L"Nelson:stl:invalidFile", L"Invalid STL file.");
                }
                appendFacet(mesh, existing, facet);
            }
        }
        return mesh;
    }
    //=============================================================================
    std::vector<Facet>
    buildFacets(const std::vector<std::array<double, 3>>& points,
        const std::vector<std::array<indexType, 3>>& connectivity,
        const std::vector<uint16>& attributes, const std::vector<double>& solidIds)
    {
        std::vector<Facet> facets;
        facets.reserve(connectivity.size());
        for (size_t k = 0; k < connectivity.size(); ++k) {
            Facet facet;
            for (size_t vertex = 0; vertex < 3; ++vertex) {
                facet.vertices[vertex] = points[connectivity[k][vertex] - 1];
            }
            facet.attribute = attributes.empty() ? 0 : attributes[k];
            facet.solidId = solidIds.empty() ? 1 : solidIds[k];
            facets.push_back(facet);
        }
        return facets;
    }
    //=============================================================================
    void
    writeBinary(const std::wstring& filename, const std::vector<Facet>& facets)
    {
        std::ofstream stream(std::filesystem::path(filename), std::ios::binary);
        if (!stream) {
            Error(L"Nelson:stl:cannotOpenFile", L"Unable to open file: %s", filename);
        }
        char header[80] = { 0 };
        std::string text = "Created by Nelson";
        std::memcpy(header, text.c_str(), text.size());
        stream.write(header, sizeof(header));
        appendUInt32(stream, static_cast<uint32>(facets.size()));
        for (const Facet& facet : facets) {
            auto normal = normalFor(facet.vertices);
            for (double value : normal) {
                appendFloat32(stream, value);
            }
            for (const auto& vertex : facet.vertices) {
                for (double value : vertex) {
                    appendFloat32(stream, value);
                }
            }
            appendUInt16(stream, facet.attribute);
        }
    }
    //=============================================================================
    std::vector<double>
    sortedSolidIds(const std::vector<Facet>& facets)
    {
        std::vector<double> ids;
        for (const Facet& facet : facets) {
            if (std::find(ids.begin(), ids.end(), facet.solidId) == ids.end()) {
                ids.push_back(facet.solidId);
            }
        }
        std::sort(ids.begin(), ids.end());
        return ids;
    }
    //=============================================================================
    void
    writeTextFacet(std::ofstream& stream, const Facet& facet)
    {
        auto normal = normalFor(facet.vertices);
        stream << "  facet normal " << normal[0] << " " << normal[1] << " " << normal[2] << "\n";
        stream << "    outer loop\n";
        for (const auto& vertex : facet.vertices) {
            stream << "      vertex " << vertex[0] << " " << vertex[1] << " " << vertex[2] << "\n";
        }
        stream << "    endloop\n";
        stream << "  endfacet\n";
    }
    //=============================================================================
    void
    writeText(const std::wstring& filename, const std::vector<Facet>& facets)
    {
        std::ofstream stream { std::filesystem::path(filename) };
        if (!stream) {
            Error(L"Nelson:stl:cannotOpenFile", L"Unable to open file: %s", filename);
        }
        stream << std::setprecision(17);
        for (double id : sortedSolidIds(facets)) {
            stream << "solid " << id << "\n";
            for (const Facet& facet : facets) {
                if (facet.solidId == id) {
                    writeTextFacet(stream, facet);
                }
            }
            stream << "endsolid " << id << "\n";
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
Mesh
readFile(const std::wstring& filename)
{
    checkExtension(filename);
    std::vector<unsigned char> bytes = readBytes(filename);
    if (isBinaryStl(bytes)) {
        return readBinary(bytes);
    }
    if (startsWithSolid(bytes)) {
        return readText(bytes);
    }
    Error(L"Nelson:stl:invalidFile", L"Invalid STL file.");
    return {};
}
//=============================================================================
void
writeFile(const std::wstring& filename, const std::string& fileFormat,
    const std::vector<std::array<double, 3>>& points,
    const std::vector<std::array<indexType, 3>>& connectivity,
    const std::vector<uint16>& attributes, const std::vector<double>& solidIds)
{
    checkExtension(filename);
    std::vector<Facet> facets = buildFacets(points, connectivity, attributes, solidIds);
    if (fileFormat == "binary") {
        writeBinary(filename, facets);
    } else if (fileFormat == "text") {
        writeText(filename, facets);
    } else {
        Error(L"Nelson:geometry:fileFormatBinaryText", L"File format must be 'binary' or 'text'.");
    }
}
//=============================================================================
ArrayOf
connectivityToArray(const std::vector<std::array<indexType, 3>>& connectivity)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(connectivity.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(connectivity.size()); ++row) {
        for (indexType col = 0; col < 3; ++col) {
            data[row + col * connectivity.size()] = static_cast<double>(connectivity[row][col]);
        }
    }
    return out;
}
//=============================================================================
ArrayOf
pointsToArray(const std::vector<std::array<double, 3>>& points)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(points.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(points.size()); ++row) {
        for (indexType col = 0; col < 3; ++col) {
            data[row + col * points.size()] = points[row][col];
        }
    }
    return out;
}
//=============================================================================
ArrayOf
attributesToArray(const std::vector<uint16>& attributes, bool binary, indexType rows)
{
    if (!binary) {
        return ArrayOf::uint16Matrix2d(rows, 0);
    }
    ArrayOf out = ArrayOf::uint16Matrix2d(static_cast<indexType>(attributes.size()), 1);
    uint16* data = static_cast<uint16*>(out.readWriteDataPointer());
    for (indexType k = 0; k < static_cast<indexType>(attributes.size()); ++k) {
        data[k] = attributes[k];
    }
    return out;
}
//=============================================================================
ArrayOf
solidIdsToArray(const std::vector<double>& solidIds)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(solidIds.size()), 1);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType k = 0; k < static_cast<indexType>(solidIds.size()); ++k) {
        data[k] = solidIds[k];
    }
    return out;
}
//=============================================================================
std::vector<std::array<indexType, 3>>
arrayToConnectivity(const ArrayOf& value, indexType pointCount)
{
    if (!value.isNumeric() || value.isComplex() || value.columns() != 3) {
        Error(L"Nelson:stl:invalidConnectivity", L"%s must be an n-by-3 numeric matrix.",
            L"Connectivity list");
    }
    std::vector<double> raw = value.asDoubleVector();
    indexType rows = value.rows();
    std::vector<std::array<indexType, 3>> connectivity(rows);
    for (indexType row = 0; row < rows; ++row) {
        for (indexType col = 0; col < 3; ++col) {
            double item = raw[row + col * rows];
            if (!std::isfinite(item) || std::floor(item) != item || item < 1 || item > pointCount) {
                Error(L"Nelson:stl:invalidVertexIndex",
                    L"Connectivity list contains an invalid vertex index.");
            }
            connectivity[row][col] = static_cast<indexType>(item);
        }
    }
    return connectivity;
}
//=============================================================================
std::vector<std::array<double, 3>>
arrayToPoints(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex() || (value.columns() != 2 && value.columns() != 3)) {
        Error(L"Nelson:stl:invalidPoints", L"%s must be an n-by-2 or n-by-3 numeric matrix.",
            L"Points");
    }
    std::vector<double> raw = value.asDoubleVector();
    indexType rows = value.rows();
    indexType cols = value.columns();
    std::vector<std::array<double, 3>> points(rows);
    for (indexType row = 0; row < rows; ++row) {
        points[row] = { raw[row], raw[row + rows], 0 };
        if (cols == 3) {
            points[row][2] = raw[row + 2 * rows];
        }
        if (!std::isfinite(points[row][0]) || !std::isfinite(points[row][1])
            || !std::isfinite(points[row][2])) {
            Error(L"Nelson:validators:mustBeFinite", L"%s must be finite.", L"Points");
        }
    }
    return points;
}
//=============================================================================
std::vector<uint16>
arrayToAttributes(const ArrayOf& value, indexType triangleCount)
{
    if (value.isEmpty()) {
        return std::vector<uint16>(triangleCount, 0);
    }
    if (value.dataClass() != NLS_UINT16 || value.elementCount() != triangleCount) {
        Error(L"Nelson:stl:invalidAttribute",
            L"%s must be a uint16 vector with one value for each triangle.", L"Attribute");
    }
    const uint16* raw = static_cast<const uint16*>(value.dataPointer());
    return std::vector<uint16>(raw, raw + triangleCount);
}
//=============================================================================
std::vector<double>
arrayToSolidIds(const ArrayOf& value, indexType triangleCount)
{
    if (value.isEmpty()) {
        return std::vector<double>(triangleCount, 1);
    }
    if (!value.isNumeric() || value.isComplex() || value.elementCount() != triangleCount) {
        Error(L"Nelson:stl:invalidSolidIndex",
            L"%s must be a numeric vector with one value for each triangle.", L"SolidIndex");
    }
    std::vector<double> raw = value.asDoubleVector();
    for (double item : raw) {
        if (!std::isfinite(item)) {
            Error(L"Nelson:validators:mustBeFinite", L"%s must be finite.", L"SolidIndex values");
        }
    }
    return raw;
}
//=============================================================================
std::wstring
textArgument(const ArrayOf& value, const std::wstring& message)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString();
    }
    Error(L"Nelson:stl:invalidArgument", message);
    return L"";
}
//=============================================================================
} // namespace Nelson::GeometryGateway::Stl
//=============================================================================
