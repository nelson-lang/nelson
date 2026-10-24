//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Code-generation support queries over an island layout: which component
// kinds the emitted embedded-Newton solver covers, and how many signal input
// ports the island reads. See physicalIslandCodegen.cpp (emitters).
//=============================================================================
#include "physicalIslandShared.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace physisland {
        //=============================================================================
        // Supported kinds: resistor, conductor, capacitor, inductor, vsource,
        // isource, currentSensor, diode, voltage / potential sensors.
        bool
        islandNewtonSupported(const IslandLayout& L)
        {
            for (const IslandComp& c : L.comps) {
                switch (c.kind) {
                case Kind::Resistor:
                case Kind::Conductor:
                case Kind::Capacitor:
                case Kind::Inductor:
                case Kind::VSource:
                case Kind::ISource:
                case Kind::CurrentSensor:
                case Kind::Diode:
                case Kind::ZDiode:
                case Kind::IdealDiode:
                case Kind::Npn:
                case Kind::Pnp:
                case Kind::Nmos:
                case Kind::Pmos:
                case Kind::Radiation:
                case Kind::Vcvs:
                case Kind::Vccs:
                case Kind::Ccvs:
                case Kind::Cccs:
                case Kind::OpAmp:
                case Kind::Short:
                case Kind::Idle:
                case Kind::Transformer:
                case Kind::Gyrator:
                case Kind::Switch:
                case Kind::VoltageSensor:
                case Kind::PotentialSensor:
                    break;
                default:
                    return false; // unsupported (variable* LTV / niche kinds)
                }
            }
            return true;
        }
        //=============================================================================
        // Number of distinct input ports (signal-driven sources) the island reads.
        int
        islandPortCount(const IslandLayout& L)
        {
            int n = 0;
            for (const IslandComp& c : L.comps) {
                if (c.port >= 0) {
                    n = (c.port + 1 > n) ? c.port + 1 : n;
                }
            }
            return n;
        }
        //=============================================================================
    } // namespace physisland
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
