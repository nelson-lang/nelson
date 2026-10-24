%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The platform tags accepted in module.json and used as registry artifact
% keys must cover every value computer('arch') can return, including the
% WebAssembly engine.
%=============================================================================
privatePath = [modulepath('modules_manager', 'functions'), '/private'];
addpath(privatePath);
%=============================================================================
platforms = nmm_valid_platforms();
asserts.istrue(iscellstr(platforms));
asserts.isequal(platforms{1}, 'all');
asserts.isequal(numel(unique(platforms)), numel(platforms));
asserts.istrue(any(strcmp(platforms, 'wasm32')));
asserts.istrue(any(strcmp(platforms, computer('arch'))));
%=============================================================================
% a wasm32-only package is installable on the WebAssembly engine only
[r, msg] = nmm_is_supported_platform({'wasm32'});
asserts.isequal(r, iswasm());
if iswasm()
  asserts.isequal(msg, '');
else
  asserts.istrue(contains(msg, 'wasm32'));
end
%=============================================================================
% a package listing wasm32 next to the running architecture is installable
[r, msg] = nmm_is_supported_platform({'wasm32', computer('arch')});
asserts.istrue(r);
asserts.isequal(msg, '');
%=============================================================================
rmpath(privatePath);
%=============================================================================
