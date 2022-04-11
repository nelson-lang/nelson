%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = which('clear');
assert_istrue(contains(p, 'libnlsMemory_manager_builtin'));
%=============================================================================
p = modulepath(nelsonroot(),'core','builtin');
assert_istrue(contains(p, 'libnlsCore_builtin'));
%=============================================================================
p = modulepath('core');
ref = [nelsonroot, '/modules/core'];
assert_isequal(p, ref);
%=============================================================================
p = modulepath(nelsonroot(),'core','etc');
ref = [nelsonroot, '/modules/core/etc'];
assert_isequal(p, ref);
%=============================================================================
p = modulepath(nelsonroot(),'core','bin');
ref = fileparts(which('clear'));
assert_isequal(p, ref);
%=============================================================================
p = modulepath(nelsonroot(),'core','root');
ref = [nelsonroot, '/modules/core/'];
assert_isequal(p, ref);
%=============================================================================
