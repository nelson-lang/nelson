//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Bridge that lets the nflow_blocks "fmuMe" block drive a real FMI 3.0 Model
// Exchange FMU, without nflow_blocks depending on fmi4c. Implements the
// IFmuMeComponent seam over FmiPackage / FmiInstance and registers the factory
// at load time. Unlike Co-Simulation, the FMU exposes only the model equations;
// the NFlow solver integrates its continuous states through the shared global
// state vector. A .fmu archive is extracted with the ZIP-slip-hardened UnZip
// into a temporary directory owned (and cleaned) by the component.
//=============================================================================
#include "FmuComponent.hpp"
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "Unzip.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <atomic>
#include <chrono>
#include <filesystem>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        //=============================================================================
        std::filesystem::path
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
        class FmuMeComponentImpl : public IFmuMeComponent
        {
            struct Port
            {
                uint32_t vr;
                int dataType; // FmiDataType as int
                int width; // array element count (1 for scalar)
            };

        public:
            FmuMeComponentImpl() = default;

            ~FmuMeComponentImpl() override
            {
                m_instance.reset();
                m_package.reset();
                if (!m_stagingDir.empty()) {
                    std::error_code ec;
                    std::filesystem::remove_all(m_stagingDir, ec);
                }
            }

            bool
            load(const std::string& fmuPath, SimDiagnostic& diag)
            {
                std::error_code ec;
                std::filesystem::path p(std::filesystem::u8path(fmuPath));
                std::string unzippedDir;

                if (std::filesystem::is_directory(p, ec)) {
                    unzippedDir = fmuPath;
                } else if (std::filesystem::is_regular_file(p, ec)) {
                    std::filesystem::path staging = makeOwnedStagingDir("nflow_fmu_me", p, ec);
                    if (staging.empty()) {
                        diag.code = "fmu_extract";
                        diag.message = "Cannot create a temporary directory to extract the FMU.";
                        diag.fatal = true;
                        return false;
                    }
                    try {
                        wstringVector entries;
                        UnZip(utf8_to_wstring(fmuPath), staging.wstring(), entries);
                    } catch (...) {
                        std::filesystem::remove_all(staging, ec);
                        diag.code = "fmu_extract";
                        diag.message = "Failed to extract the FMU archive '" + fmuPath + "'.";
                        diag.fatal = true;
                        return false;
                    }
                    m_stagingDir = staging.string();
                    unzippedDir = m_stagingDir;
                } else {
                    diag.code = "fmu_not_found";
                    diag.message = "FMU path is neither a directory nor a file: '" + fmuPath + "'.";
                    diag.fatal = true;
                    return false;
                }

                std::string err;
                m_package = Fmi::FmiPackage::loadUnzipped("nflow_fmu_me", unzippedDir, err);
                if (!m_package) {
                    diag.code = "fmu_load";
                    diag.message
                        = err.empty() ? ("Failed to load FMU from '" + fmuPath + "'.") : err;
                    diag.fatal = true;
                    return false;
                }
                const Fmi::FmiModelInfo& mi = m_package->info();
                if (!mi.supportsModelExchange) {
                    diag.code = "fmu_no_me";
                    diag.message = "FMU does not provide the Model Exchange interface.";
                    diag.fatal = true;
                    return false;
                }
                for (const Fmi::FmiVariable& v : mi.variables) {
                    if (!isSupportedDataType(v.dataType)) {
                        continue;
                    }
                    const int width = std::max(1, v.arraySize);
                    if (v.causality == Fmi::FmiCausality::Output) {
                        m_outputVr.push_back(
                            { v.valueReference, static_cast<int>(v.dataType), width });
                    } else if (v.causality == Fmi::FmiCausality::Input) {
                        m_inputVr.push_back(
                            { v.valueReference, static_cast<int>(v.dataType), width });
                    } else if (v.causality == Fmi::FmiCausality::Parameter) {
                        m_paramVrByName[v.name]
                            = { v.valueReference, static_cast<int>(v.dataType) };
                    } else if (v.causality == Fmi::FmiCausality::StructuralParameter) {
                        m_structVrByName[v.name]
                            = { v.valueReference, static_cast<int>(v.dataType) };
                    }
                }
                return true;
            }

            // Structural parameters are set in Configuration Mode (between
            // instantiate and enterInitializationMode); array ports whose dimension
            // is bound to one are resized accordingly.
            bool
            applyStructuralParameters(const std::vector<std::string>& parameterNames,
                const std::vector<double>& parameterValues, SimDiagnostic& diag)
            {
                std::vector<std::pair<uint32_t, uint64_t>> overridesByVr;
                std::vector<size_t> structIdx;
                for (size_t i = 0; i < parameterNames.size(); ++i) {
                    auto it = m_structVrByName.find(parameterNames[i]);
                    if (it != m_structVrByName.end()) {
                        structIdx.push_back(i);
                        overridesByVr.emplace_back(
                            it->second.first, (uint64_t)std::llround(parameterValues[i]));
                    }
                }
                if (!structIdx.empty()) {
                    std::string err;
                    if (!m_instance->enterConfigurationMode(err)) {
                        diag.code = "fmu_config";
                        diag.message = err;
                        diag.fatal = true;
                        return false;
                    }
                    for (size_t i : structIdx) {
                        auto it = m_structVrByName.find(parameterNames[i]);
                        if (!m_instance->setTyped(it->second.second, { it->second.first },
                                { parameterValues[i] }, err)) {
                            diag.code = "fmu_set_structural";
                            diag.message = err;
                            diag.fatal = true;
                            return false;
                        }
                    }
                    if (!m_instance->exitConfigurationMode(err)) {
                        diag.code = "fmu_config";
                        diag.message = err;
                        diag.fatal = true;
                        return false;
                    }
                    for (Port& p : m_inputVr) {
                        refreshPortWidth(p, overridesByVr);
                    }
                    for (Port& p : m_outputVr) {
                        refreshPortWidth(p, overridesByVr);
                    }
                }
                return true;
            }

            void
            refreshPortWidth(
                Port& p, const std::vector<std::pair<uint32_t, uint64_t>>& overridesByVr)
            {
                for (const Fmi::FmiVariable& v : m_package->info().variables) {
                    if (v.valueReference == p.vr) {
                        p.width = std::max(1, Fmi::effectiveArraySize(v, overridesByVr));
                        return;
                    }
                }
            }

            static bool
            isSupportedDataType(Fmi::FmiDataType t)
            {
                switch (t) {
                case Fmi::FmiDataType::Float64:
                case Fmi::FmiDataType::Float32:
                case Fmi::FmiDataType::Int8:
                case Fmi::FmiDataType::UInt8:
                case Fmi::FmiDataType::Int16:
                case Fmi::FmiDataType::UInt16:
                case Fmi::FmiDataType::Int32:
                case Fmi::FmiDataType::UInt32:
                case Fmi::FmiDataType::Int64:
                case Fmi::FmiDataType::UInt64:
                case Fmi::FmiDataType::Boolean:
                case Fmi::FmiDataType::Enumeration:
                    return true;
                default:
                    return false;
                }
            }

            bool
            initialize(double startTime, double stopTime,
                const std::vector<std::string>& parameterNames,
                const std::vector<double>& parameterValues, SimDiagnostic& diag) override
            {
                std::string err;
                m_instance = m_package->instantiateModelExchange("nflow_fmu_me", false, err);
                if (!m_instance) {
                    diag.code = "fmu_instantiate";
                    diag.message = err.empty() ? "fmi3InstantiateModelExchange failed." : err;
                    diag.fatal = true;
                    return false;
                }
                if (!applyStructuralParameters(parameterNames, parameterValues, diag)) {
                    return false;
                }
                const bool stopDefined = stopTime > startTime;
                const bool tolDefined = m_relTol > 0.0;
                if (!m_instance->enterInitializationMode(tolDefined, tolDefined ? m_relTol : 0.0,
                        startTime, stopDefined, stopTime, err)) {
                    diag.code = "fmu_init";
                    diag.message = err;
                    diag.fatal = true;
                    return false;
                }
                for (size_t i = 0; i < parameterNames.size(); ++i) {
                    auto it = m_paramVrByName.find(parameterNames[i]);
                    if (it == m_paramVrByName.end()) {
                        continue;
                    }
                    if (!m_instance->setTyped(
                            it->second.second, { it->second.first }, { parameterValues[i] }, err)) {
                        diag.code = "fmu_set_param";
                        diag.message = err;
                        diag.fatal = true;
                        return false;
                    }
                }
                if (!m_instance->exitInitializationMode(err)) {
                    diag.code = "fmu_init";
                    diag.message = err;
                    diag.fatal = true;
                    return false;
                }
                if (!m_instance->enterContinuousTimeMode(err)) {
                    diag.code = "fmu_continuous_mode";
                    diag.message = err;
                    diag.fatal = true;
                    return false;
                }
                m_nx = static_cast<int>(m_instance->numberOfContinuousStates());
                m_nz = static_cast<int>(m_instance->numberOfEventIndicators());
                m_x0.assign(static_cast<size_t>(m_nx > 0 ? m_nx : 0), 0.0);
                if (m_nx > 0) {
                    std::vector<double> x;
                    if (m_instance->getContinuousStates(x, static_cast<size_t>(m_nx), err)) {
                        for (int i = 0; i < m_nx && i < static_cast<int>(x.size()); ++i) {
                            m_x0[static_cast<size_t>(i)] = x[static_cast<size_t>(i)];
                        }
                    }
                }
                return true;
            }

            int
            numContinuousStates() const override
            {
                return m_nx;
            }
            int
            numEventIndicators() const override
            {
                return m_nz;
            }
            void
            getInitialStates(double* x, int n) const override
            {
                const int m = std::min(n, static_cast<int>(m_x0.size()));
                for (int i = 0; i < m; ++i) {
                    x[i] = m_x0[static_cast<size_t>(i)];
                }
            }

            int
            numInputs() const override
            {
                return static_cast<int>(m_inputVr.size());
            }
            int
            numOutputs() const override
            {
                return static_cast<int>(m_outputVr.size());
            }
            int
            inputPortWidth(int p) const override
            {
                return (p >= 0 && p < (int)m_inputVr.size()) ? m_inputVr[p].width : 1;
            }
            int
            outputPortWidth(int p) const override
            {
                return (p >= 0 && p < (int)m_outputVr.size()) ? m_outputVr[p].width : 1;
            }

            bool
            setInputs(const double* values, int n, SimDiagnostic& diag) override
            {
                if (n <= 0 || m_inputVr.empty() || !m_instance) {
                    return true;
                }
                std::string err;
                int at = 0;
                for (const Port& port : m_inputVr) {
                    if (at >= n) {
                        break;
                    }
                    const int w = std::min(port.width, n - at);
                    std::vector<double> vals(values + at, values + at + w);
                    if (!m_instance->setTyped(port.dataType, { port.vr }, vals, err)) {
                        diag.code = "fmu_set";
                        diag.message = err;
                        return false;
                    }
                    at += port.width;
                }
                return true;
            }

            bool
            getOutputs(double* out, int n, SimDiagnostic& diag) override
            {
                if (n <= 0 || m_outputVr.empty() || !m_instance) {
                    return true;
                }
                std::string err;
                int at = 0;
                for (const Port& port : m_outputVr) {
                    if (at >= n) {
                        break;
                    }
                    std::vector<double> value;
                    if (!m_instance->getTyped(port.dataType, { port.vr }, value, err, port.width)) {
                        diag.code = "fmu_get";
                        diag.message = err;
                        return false;
                    }
                    const int w = std::min(port.width, n - at);
                    for (int k = 0; k < w; ++k) {
                        out[at + k] = (k < (int)value.size()) ? value[k] : 0.0;
                    }
                    at += port.width;
                }
                return true;
            }

            bool
            setStateTime(double time, const double* x, int n, SimDiagnostic& diag) override
            {
                if (!m_instance) {
                    return false;
                }
                std::string err;
                if (!m_instance->setTime(time, err)) {
                    diag.code = "fmu_set_time";
                    diag.message = err;
                    return false;
                }
                if (m_nx > 0) {
                    std::vector<double> states(static_cast<size_t>(m_nx), 0.0);
                    const int m = std::min(n, m_nx);
                    for (int i = 0; i < m; ++i) {
                        states[static_cast<size_t>(i)] = x[i];
                    }
                    if (!m_instance->setContinuousStates(states, err)) {
                        diag.code = "fmu_set_states";
                        diag.message = err;
                        return false;
                    }
                }
                return true;
            }

            bool
            getDerivatives(double* xdot, int n, SimDiagnostic& diag) override
            {
                if (!m_instance || m_nx <= 0) {
                    return true;
                }
                std::string err;
                std::vector<double> dx;
                if (!m_instance->getDerivatives(dx, static_cast<size_t>(m_nx), err)) {
                    diag.code = "fmu_derivatives";
                    diag.message = err;
                    return false;
                }
                const int m = std::min(n, static_cast<int>(dx.size()));
                for (int i = 0; i < m; ++i) {
                    xdot[i] = dx[static_cast<size_t>(i)];
                }
                return true;
            }

            bool
            getEventIndicators(double* g, int n, SimDiagnostic& diag) override
            {
                if (!m_instance || m_nz <= 0) {
                    return true;
                }
                std::string err;
                std::vector<double> gi;
                if (!m_instance->getEventIndicators(gi, static_cast<size_t>(m_nz), err)) {
                    diag.code = "fmu_event_indicators";
                    diag.message = err;
                    return false;
                }
                const int m = std::min(n, static_cast<int>(gi.size()));
                for (int i = 0; i < m; ++i) {
                    g[i] = gi[static_cast<size_t>(i)];
                }
                return true;
            }

            bool
            handleEvent(const double* xin, double* xout, int n, bool& statesChanged,
                SimDiagnostic& diag) override
            {
                statesChanged = false;
                // Default: pass the crossing state through unchanged.
                const int m = std::min(n, m_nx);
                for (int i = 0; i < m; ++i) {
                    xout[i] = xin[i];
                }
                if (!m_instance) {
                    return false;
                }
                std::string err;
                std::vector<double> states(static_cast<size_t>(m_nx > 0 ? m_nx : 0), 0.0);
                for (int i = 0; i < m; ++i) {
                    states[static_cast<size_t>(i)] = xin[i];
                }
                if (m_nx > 0 && !m_instance->setContinuousStates(states, err)) {
                    diag.code = "fmu_set_states";
                    diag.message = err;
                    return false;
                }
                if (!m_instance->enterEventMode(err)) {
                    diag.code = "fmu_event_mode";
                    diag.message = err;
                    return false;
                }
                bool valuesChanged = false;
                bool terminate = false;
                if (!m_instance->updateDiscreteStates(valuesChanged, terminate, err)) {
                    diag.code = "fmu_update_discrete";
                    diag.message = err;
                    return false;
                }
                if (valuesChanged && m_nx > 0) {
                    std::vector<double> x;
                    if (m_instance->getContinuousStates(x, static_cast<size_t>(m_nx), err)) {
                        const int k = std::min(n, static_cast<int>(x.size()));
                        for (int i = 0; i < k; ++i) {
                            xout[i] = x[static_cast<size_t>(i)];
                        }
                        statesChanged = true;
                    }
                }
                if (!m_instance->enterContinuousTimeMode(err)) {
                    diag.code = "fmu_continuous_mode";
                    diag.message = err;
                    return false;
                }
                return true;
            }

            void
            setSolverTolerance(double relTol) override
            {
                m_relTol = relTol;
            }

        private:
            double m_relTol = 0.0; // diagram relative tolerance (0 => FMU default)
            std::string m_stagingDir;
            std::unique_ptr<Fmi::FmiPackage> m_package;
            std::unique_ptr<Fmi::FmiInstance> m_instance;
            std::vector<Port> m_inputVr;
            std::vector<Port> m_outputVr;
            std::unordered_map<std::string, std::pair<uint32_t, int>> m_paramVrByName;
            // Structural parameters (settable only in Configuration Mode; array
            // dimensions may be bound to their values).
            std::unordered_map<std::string, std::pair<uint32_t, int>> m_structVrByName;
            std::vector<double> m_x0;
            int m_nx = 0;
            int m_nz = 0;
        };
        //=============================================================================
        struct FmuMeFactoryRegistrar
        {
            FmuMeFactoryRegistrar()
            {
                registerFmuMeComponentFactory(
                    [](const std::string& fmuPath,
                        SimDiagnostic& diag) -> std::unique_ptr<IFmuMeComponent> {
                        auto comp = std::make_unique<FmuMeComponentImpl>();
                        if (!comp->load(fmuPath, diag)) {
                            return nullptr;
                        }
                        return comp;
                    });
            }
        };
        //=============================================================================
        static FmuMeFactoryRegistrar g_fmuMeFactoryRegistrar;
        //=============================================================================
    } // namespace
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
