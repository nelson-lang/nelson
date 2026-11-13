%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% rehash reloads function, script, private and class files changed on disk.
%=============================================================================
previousPath = path();
T = tempname();
mkdir(T);
mkdir(fullfile(T, 'private'));
filewrite(fullfile(T, 'rehash_probe_fun.m'), {'function r = rehash_probe_fun()'; 'r = 1;'; 'end'});
filewrite(fullfile(T, 'rehash_probe_script.m'), {'rehash_probe_value = 1;'});
filewrite(fullfile(T, 'rehash_probe_cls.m'), ...
  {'classdef rehash_probe_cls'; 'methods (Static)'; 'function r = v()'; 'r = 1;'; 'end'; 'end'; 'end'});
filewrite(fullfile(T, 'rehash_probe_caller.m'), ...
  {'function r = rehash_probe_caller()'; 'r = rehash_probe_priv();'; 'end'});
filewrite(fullfile(T, 'private', 'rehash_probe_priv.m'), {'function r = rehash_probe_priv()'; 'r = 1;'; 'end'});
addpath(T);
%=============================================================================
rehash_probe_script;
asserts.isequal(rehash_probe_fun(), 1);
asserts.isequal(rehash_probe_value, 1);
asserts.isequal(rehash_probe_cls.v(), 1);
asserts.isequal(rehash_probe_caller(), 1);
%=============================================================================
filewrite(fullfile(T, 'rehash_probe_fun.m'), {'function r = rehash_probe_fun()'; 'r = 33;'; 'end'});
filewrite(fullfile(T, 'rehash_probe_script.m'), {'rehash_probe_value = 33;'});
filewrite(fullfile(T, 'rehash_probe_cls.m'), ...
  {'classdef rehash_probe_cls'; 'methods (Static)'; 'function r = v()'; 'r = 33;'; 'end'; 'end'; 'end'});
filewrite(fullfile(T, 'private', 'rehash_probe_priv.m'), {'function r = rehash_probe_priv()'; 'r = 33;'; 'end'});
rehash();
rehash_probe_script;
asserts.isequal(rehash_probe_fun(), 33);
asserts.isequal(rehash_probe_value, 33);
asserts.isequal(rehash_probe_cls.v(), 33);
asserts.isequal(rehash_probe_caller(), 33);
%=============================================================================
% same size, new content: the write time tells the change
filewrite(fullfile(T, 'rehash_probe_fun.m'), {'function r = rehash_probe_fun()'; 'r = 44;'; 'end'});
rehash path
asserts.isequal(rehash_probe_fun(), 44);
filewrite(fullfile(T, 'rehash_probe_fun.m'), {'function r = rehash_probe_fun()'; 'r = 55;'; 'end'});
rehash('toolboxcache');
asserts.isequal(rehash_probe_fun(), 55);
%=============================================================================
% an unchanged file keeps its loaded definition
rehash();
asserts.isequal(rehash_probe_fun(), 55);
%=============================================================================
asserts.checkerror('rehash(''unknown'')', ...
  _('Option must be ''path'', ''toolbox'', ''pathreset'', ''toolboxreset'' or ''toolboxcache''.'), ...
  'Nelson:rehash:invalidOption');
asserts.checkerror('rehash(3)', ...
  _('Option must be ''path'', ''toolbox'', ''pathreset'', ''toolboxreset'' or ''toolboxcache''.'), ...
  'Nelson:rehash:invalidOption');
%=============================================================================
path(previousPath);
clear('rehash_probe_fun', 'rehash_probe_caller', 'rehash_probe_cls');
rmdir(T, 's');
%=============================================================================
