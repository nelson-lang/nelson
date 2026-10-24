%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build the 'cumprod' native block plugin with Nelson's own build tools.
%
% It relies on the dynamic_link module (dlgeneratemake / dlmake) and on the
% nflow SDK resolvers (dlgetnflowincludes / dlgetnflowlibraries), so it needs
% no hand-written CMakeLists.txt and works both from the Nelson source tree and
% from an installation.
%
% Usage (from this directory, inside Nelson):
%   builder
%
% On success the shared library is written next to library.json with the name
% declared in its "native" field (nflow_cumprod.dll / .so / .dylib).
%=============================================================================
function builder()
  if ~ismodule('dynamic_link')
    error('Nelson:nflow:cumprod:dynamicLinkRequired', ...
      _('The dynamic_link module is required to build this plugin.'));
  end
  if ispc() && ~havecompiler()
    configuremsvc();
  end
  if ~havecompiler()
    error('Nelson:nflow:cumprod:noCompiler', ...
      _('No C++ compiler configured. See: doc supported_compiler.'));
  end

  exampleDir = fileparts(mfilename('fullpath'));
  libraryName = readNativeName([exampleDir, '/library.json']);
  source = [exampleDir, '/', 'cumprod.cpp'];

  % dlmake writes its generated CMakeLists.txt and intermediate files into the
  % build directory, so build in a scratch folder and copy the result back,
  % keeping the example directory clean.
  buildDir = [tempname(), '_nflow_cumprod'];
  mkdir(buildDir);
  cleaner = onCleanup(@() rmdir(buildDir, 's')); %#ok<NASGU>

  [ok, msg] = dlgeneratemake('dynamic_library', buildDir, libraryName, ...
    {source}, dlgetnflowincludes(), {}, dlgetnflowlibraries());
  if ~ok
    error('Nelson:nflow:cumprod:generateFailed', '%s', msg);
  end

  [ok, msg] = dlmake(buildDir);
  if ~ok
    error('Nelson:nflow:cumprod:buildFailed', '%s', msg);
  end

  built = [buildDir, '/', libraryName, getdynlibext()];
  if ~isfile(built)
    error('Nelson:nflow:cumprod:missingOutput', ...
      _('Build reported success but %s was not produced.'), built);
  end
  destination = [exampleDir, '/', libraryName, getdynlibext()];
  copyfile(built, destination, 'f');
  disp([_('Built native block plugin: '), destination]);
end
%=============================================================================
function name = readNativeName(libraryJson)
  % The output filename must match the "native" field of library.json.
  if ~isfile(libraryJson)
    error('Nelson:nflow:cumprod:missingManifest', ...
      _('library.json not found next to builder.m.'));
  end
  manifest = jsondecode(fileread(libraryJson));
  if ~isfield(manifest, 'native') || isempty(manifest.native)
    error('Nelson:nflow:cumprod:missingNativeField', ...
      _('library.json has no "native" field.'));
  end
  name = manifest.native;
end
%=============================================================================
