%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = iswasm();
%=============================================================================
asserts.istrue(islogical(r));
asserts.istrue(isscalar(r));
%=============================================================================
% Exactly one of ispc/ismac/isunix is true on a native build; a WebAssembly
% build reports iswasm() == true.
if iswasm()
  asserts.isfalse(ispc());
else
  asserts.istrue(ispc() || ismac() || isunix());
end
%=============================================================================
assert_checkerror('iswasm(1)', _('Too many input arguments.'));
%=============================================================================
