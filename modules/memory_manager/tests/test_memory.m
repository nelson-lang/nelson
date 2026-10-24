%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('memory'), 0);
asserts.isequal(nargout('memory'), 2);
%=============================================================================
[userMemory, systemMemory] = memory();
asserts.istrue(isstruct(userMemory));
asserts.istrue(isfield(userMemory, 'MaxPossibleArrayBytes'));
asserts.istrue(isfield(userMemory, 'MemAvailableAllArrays'));
asserts.istrue(isfield(userMemory, 'MemUsedNelson'));
asserts.istrue(isstruct(systemMemory));
asserts.istrue(isfield(systemMemory, 'VirtualAddressSpace'));
asserts.istrue(isfield(systemMemory, 'SystemMemory'));
asserts.istrue(isfield(systemMemory, 'PhysicalMemory'));
%=============================================================================
