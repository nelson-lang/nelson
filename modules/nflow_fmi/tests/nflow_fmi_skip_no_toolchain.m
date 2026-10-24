%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% True when no usable C toolchain is reachable to build an FMU binary.
%
% The bundled external reference FMU (VanDerPol.fmu) ships source plus
% binaries for other platforms only; on this platform nflow compiles it from
% source at load time (fmi3Instantiate on a freshly built platform binary),
% which needs a C compiler and cmake. When Nelson runs from an installed
% bundle its PATH frequently omits cmake, so findcmake() reports none and the
% FMU cannot be instantiated: the instantiation tests then skip cleanly rather
% than fail on a missing toolchain.
%=============================================================================
function tf = nflow_fmi_skip_no_toolchain()
  [hasCompiler, ~] = havecompiler();
  hasCmake = findcmake();
  tf = ~(hasCompiler && hasCmake);
end
%=============================================================================
