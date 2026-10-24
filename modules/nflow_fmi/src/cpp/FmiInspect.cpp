//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FmiInspect.hpp"
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "Unzip.hpp"
#include <atomic>
#include <chrono>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace Fmi {
        //=============================================================================
        static std::filesystem::path
        makeOwnedStagingDir(
            const char* prefix, const std::filesystem::path& source, std::error_code& ec)
        {
            static std::atomic<unsigned long long> counter { 0 };
            const auto base = std::filesystem::temp_directory_path(ec);
            const auto tick = std::chrono::steady_clock::now().time_since_epoch().count();
            const std::string stem = source.stem().u8string();
            for (int attempt = 0; attempt < 64; ++attempt) {
                std::filesystem::path staging = base
                    / (std::string(prefix) + "_" + std::to_string(tick) + "_"
                        + std::to_string(counter.fetch_add(1)) + "_" + stem);
                ec.clear();
                if (std::filesystem::create_directory(staging, ec)) {
                    return staging;
                }
            }
            return {};
        }
        //=============================================================================
        static const char*
        causalityToString(FmiCausality c)
        {
            switch (c) {
            case FmiCausality::Parameter:
                return "parameter";
            case FmiCausality::CalculatedParameter:
                return "calculatedParameter";
            case FmiCausality::Input:
                return "input";
            case FmiCausality::Output:
                return "output";
            case FmiCausality::Local:
                return "local";
            case FmiCausality::Independent:
                return "independent";
            case FmiCausality::StructuralParameter:
                return "structuralParameter";
            default:
                return "unknown";
            }
        }
        //=============================================================================
        static const char*
        dataTypeToString(FmiDataType t)
        {
            switch (t) {
            case FmiDataType::Float64:
                return "Float64";
            case FmiDataType::Float32:
                return "Float32";
            case FmiDataType::Int64:
                return "Int64";
            case FmiDataType::Int32:
                return "Int32";
            case FmiDataType::Int16:
                return "Int16";
            case FmiDataType::Int8:
                return "Int8";
            case FmiDataType::UInt64:
                return "UInt64";
            case FmiDataType::UInt32:
                return "UInt32";
            case FmiDataType::UInt16:
                return "UInt16";
            case FmiDataType::UInt8:
                return "UInt8";
            case FmiDataType::Boolean:
                return "Boolean";
            case FmiDataType::String:
                return "String";
            case FmiDataType::Binary:
                return "Binary";
            case FmiDataType::Clock:
                return "Clock";
            case FmiDataType::Enumeration:
                return "Enumeration";
            default:
                return "Unknown";
            }
        }
        //=============================================================================
        static std::string
        jsonEscape(const std::string& s)
        {
            std::string out;
            out.reserve(s.size() + 8);
            for (char c : s) {
                switch (c) {
                case '"':
                    out += "\\\"";
                    break;
                case '\\':
                    out += "\\\\";
                    break;
                case '\n':
                    out += "\\n";
                    break;
                case '\r':
                    out += "\\r";
                    break;
                case '\t':
                    out += "\\t";
                    break;
                default:
                    if (static_cast<unsigned char>(c) < 0x20) {
                        char buf[8];
                        std::snprintf(buf, sizeof(buf), "\\u%04x", c);
                        out += buf;
                    } else {
                        out += c;
                    }
                }
            }
            return out;
        }
        //=============================================================================
        static std::string
        errorJson(const std::string& message)
        {
            return "{\"success\":false,\"error\":\"" + jsonEscape(message) + "\"}";
        }
        //=============================================================================
        static std::string
        base64Encode(const std::vector<unsigned char>& data)
        {
            static const char table[]
                = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
            std::string out;
            out.reserve(((data.size() + 2) / 3) * 4);
            size_t i = 0;
            for (; i + 2 < data.size(); i += 3) {
                const unsigned int n = (data[i] << 16) | (data[i + 1] << 8) | data[i + 2];
                out += table[(n >> 18) & 63];
                out += table[(n >> 12) & 63];
                out += table[(n >> 6) & 63];
                out += table[n & 63];
            }
            if (i < data.size()) {
                unsigned int n = data[i] << 16;
                const bool two = (i + 1 < data.size());
                if (two) {
                    n |= data[i + 1] << 8;
                }
                out += table[(n >> 18) & 63];
                out += table[(n >> 12) & 63];
                out += two ? table[(n >> 6) & 63] : '=';
                out += '=';
            }
            return out;
        }
        //=============================================================================
        // FMI defines the optional model icon as model.png in the FMU root. Some tools
        // ship model.svg. Return it as a data URI (base64) so it survives the temporary
        // extraction directory being removed, or "" when the FMU has no icon. Icons
        // larger than ~256 KB are ignored to keep the .nflow small.
        //=============================================================================
        static std::string
        iconDataUri(const std::filesystem::path& dir)
        {
            struct IconCandidate
            {
                const char* file;
                const char* mime;
            };
            static const IconCandidate candidates[]
                = { { "model.png", "image/png" }, { "model.svg", "image/svg+xml" } };
            std::error_code ec;
            for (const IconCandidate& c : candidates) {
                std::filesystem::path iconPath = dir / c.file;
                if (!std::filesystem::is_regular_file(iconPath, ec)) {
                    continue;
                }
                const auto size = std::filesystem::file_size(iconPath, ec);
                if (ec || size == 0 || size > 256u * 1024u) {
                    continue;
                }
                std::ifstream ifs(iconPath, std::ios::binary);
                if (!ifs) {
                    continue;
                }
                std::vector<unsigned char> bytes(
                    (std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
                if (bytes.empty()) {
                    continue;
                }
                return std::string("data:") + c.mime + ";base64," + base64Encode(bytes);
            }
            return std::string();
        }
        //=============================================================================
        std::string
        inspectFmuJson(const std::string& fmuPath, std::string& errorMessage)
        {
            errorMessage.clear();
            std::error_code ec;
            std::filesystem::path p(std::filesystem::u8path(fmuPath));
            std::string unzippedDir;
            std::string stagingDir;

            if (std::filesystem::is_directory(p, ec)) {
                unzippedDir = fmuPath;
            } else if (std::filesystem::is_regular_file(p, ec)) {
                std::filesystem::path staging = makeOwnedStagingDir("nflow_fmu_inspect", p, ec);
                if (staging.empty()) {
                    errorMessage = "Cannot create a temporary directory to extract the FMU.";
                    return errorJson(errorMessage);
                }
                try {
                    wstringVector entries;
                    UnZip(std::filesystem::u8path(fmuPath).wstring(), staging.wstring(), entries);
                } catch (...) {
                    std::filesystem::remove_all(staging, ec);
                    errorMessage = "Failed to extract the FMU archive '" + fmuPath + "'.";
                    return errorJson(errorMessage);
                }
                stagingDir = staging.string();
                unzippedDir = stagingDir;
            } else {
                errorMessage = "FMU path is neither a directory nor a file: '" + fmuPath + "'.";
                return errorJson(errorMessage);
            }

            std::string err;
            std::unique_ptr<FmiPackage> pkg
                = FmiPackage::loadUnzipped("nflow_inspect", unzippedDir, err);
            if (!pkg) {
                if (!stagingDir.empty()) {
                    std::filesystem::remove_all(stagingDir, ec);
                }
                errorMessage = err.empty() ? ("Failed to load FMU from '" + fmuPath + "'.") : err;
                return errorJson(errorMessage);
            }

            const FmiModelInfo& mi = pkg->info();
            std::ostringstream os;
            os << "{\"success\":true";
            os << ",\"modelIdentifier\":\"" << jsonEscape(mi.modelIdentifier) << "\"";
            os << ",\"fmiVersion\":\"" << jsonEscape(mi.fmiVersion) << "\"";
            os << ",\"coSimulation\":" << (mi.supportsCoSimulation ? "true" : "false");
            os << ",\"modelExchange\":" << (mi.supportsModelExchange ? "true" : "false");
            os << ",\"scheduledExecution\":" << (mi.supportsScheduledExecution ? "true" : "false");
            os << ",\"variables\":[";
            for (size_t i = 0; i < mi.variables.size(); ++i) {
                const FmiVariable& v = mi.variables[i];
                if (i) {
                    os << ",";
                }
                os << "{\"name\":\"" << jsonEscape(v.name) << "\"";
                os << ",\"valueReference\":" << v.valueReference;
                os << ",\"causality\":\"" << causalityToString(v.causality) << "\"";
                os << ",\"dataType\":\"" << dataTypeToString(v.dataType) << "\"";
                os << ",\"description\":\"" << jsonEscape(v.description) << "\"";
                if (v.arraySize > 1) {
                    os << ",\"arraySize\":" << v.arraySize;
                }
                // Per-dimension descriptors: {"start":n} for a fixed size,
                // {"vr":n,"start":n} for a size bound to a structural parameter
                // (start = the parameter's default). Lets the editor recompute
                // effective port widths when a structural parameter is overridden.
                if (!v.dimensions.empty()) {
                    os << ",\"dimensions\":[";
                    for (size_t d = 0; d < v.dimensions.size(); ++d) {
                        if (d) {
                            os << ",";
                        }
                        const FmiDimension& dim = v.dimensions[d];
                        if (dim.hasStart) {
                            os << "{\"start\":" << dim.start << "}";
                        } else {
                            os << "{\"vr\":" << dim.valueReference << ",\"start\":" << dim.start
                               << "}";
                        }
                    }
                    os << "]";
                }
                if (v.hasStartValue) {
                    os << ",\"startValue\":" << v.startValue;
                }
                os << "}";
            }
            os << "]";
            // The fmuMe block needs the continuous-state and event-indicator counts at
            // state-layout time. They come from the FMU functions, so briefly instantiate a
            // Model Exchange instance and query them. Absent when the FMU is not ME or the
            // query fails; the block then reports "no continuous states".
            if (mi.supportsModelExchange) {
                std::string ierr;
                std::unique_ptr<FmiInstance> inst
                    = pkg->instantiateModelExchange("nflow_inspect_me", false, ierr);
                if (inst && inst->enterInitializationMode(false, 0.0, 0.0, true, 1.0, ierr)
                    && inst->exitInitializationMode(ierr) && inst->enterContinuousTimeMode(ierr)) {
                    os << ",\"numberOfContinuousStates\":" << inst->numberOfContinuousStates();
                    os << ",\"numberOfEventIndicators\":" << inst->numberOfEventIndicators();
                    std::string terr;
                    inst->terminate(terr);
                }
            }
            const std::string icon = iconDataUri(std::filesystem::u8path(unzippedDir));
            if (!icon.empty()) {
                os << ",\"icon\":\"" << jsonEscape(icon) << "\"";
            }
            os << "}";

            // pkg destroyed before removing the extraction directory.
            pkg.reset();
            if (!stagingDir.empty()) {
                std::filesystem::remove_all(stagingDir, ec);
            }
            return os.str();
        }
        //=============================================================================
    } // namespace Fmi
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
