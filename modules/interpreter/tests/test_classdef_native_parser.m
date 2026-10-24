%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Native classdef parser, shadow parity. The token-driven parser is
% built alongside the text scanner, which stays authoritative until parity is
% proven. This is the differential gate for the header stage: for every classdef
% fixture, the native header parse (__classdef_native_header__) must produce the
% same class name and superclass list as the text scanner (via
% __classdef_parse_dump__).
%=============================================================================
classdef_dir = [modulepath('interpreter', 'tests'), '/classdef'];
files = dir([classdef_dir, '/*.m']);
checked = 0;
for k = 1:numel(files)
  source = fileread([files(k).folder, '/', files(k).name]);
  if ~strcmp(parsestring(source), 'classdef')
    continue;
  end
  dump = strsplit(__classdef_introspect__('parse_dump', source), sprintf('\n'));
  scanner_name = strrep(dump{find(startsWith(dump, 'class='), 1)}, 'class=', '');
  scanner_super = strrep(dump{find(startsWith(dump, 'superclasses='), 1)}, 'superclasses=', '');
  native = strsplit(__classdef_introspect__('native_header', source), sprintf('\n'));
  asserts.isfalse(startsWith(native{1}, 'error='));
  native_name = strrep(native{1}, 'name=', '');
  native_super = strrep(native{2}, 'superclasses=', '');
  asserts.isequal(native_name, scanner_name);
  asserts.isequal(native_super, scanner_super);
  checked = checked + 1;
end
asserts.istrue(checked > 10);
%=============================================================================
% Direct spot checks of the native header parse.
n = strsplit(__classdef_introspect__('native_header', 'classdef (Abstract) Foo < handle & pkg.mixin.Copyable'), sprintf('\n'));
asserts.isequal(strrep(n{1}, 'name=', ''), 'Foo');
asserts.isequal(strrep(n{2}, 'superclasses=', ''), 'handle,pkg.mixin.Copyable');
%=============================================================================
n = strsplit(__classdef_introspect__('native_header', 'classdef Simple'), sprintf('\n'));
asserts.isequal(strrep(n{1}, 'name=', ''), 'Simple');
asserts.isequal(strrep(n{2}, 'superclasses=', ''), '');
%=============================================================================
% Skeleton stage: the native structural parse must match the text scanner on
% every classdef fixture. Both skeletons are rendered in C++ (native and
% scanner) so the comparison is a plain string equality, robust to the newlines
% that multi-line property defaults embed. The skeleton covers the class name,
% superclasses, ordered property names with their default and validation
% expressions, and the ordered method/event/enumeration member names.
checked = 0;
for k = 1:numel(files)
  source = fileread([files(k).folder, '/', files(k).name]);
  if ~strcmp(parsestring(source), 'classdef')
    continue;
  end
  native = __classdef_introspect__('native_skeleton', source);
  scanner = __classdef_introspect__('scanner_skeleton', source);
  asserts.isfalse(startsWith(native, 'error='));
  asserts.isequal(native, scanner);
  checked = checked + 1;
end
asserts.istrue(checked > 10);
%=============================================================================
% Full definition stage: the native parser assembles a complete
% ClassdefDefinition. Its canonical dump must match the scanner on every
% behaviorally-significant field. The only tolerated differences are the
% diagnostic method 'line' and 'srclen' (raw vs expanded line numbers, and
% whitespace in method source that re-parses to the same function), which are
% normalized out before comparison.
checked = 0;
for k = 1:numel(files)
  source = fileread([files(k).folder, '/', files(k).name]);
  if ~strcmp(parsestring(source), 'classdef')
    continue;
  end
  native = regexprep(__classdef_introspect__('native_full_dump', source), ';line=\d+;srclen=\d+', ';line=;srclen=');
  scanner = regexprep(__classdef_introspect__('parse_dump', source), ';line=\d+;srclen=\d+', ';line=;srclen=');
  asserts.isfalse(startsWith(native, 'error='));
  asserts.isequal(native, scanner);
  checked = checked + 1;
end
asserts.istrue(checked > 10);
%=============================================================================
% Rejection parity: for every malformed classdef fixture, the native parser must
% reject it with the same error the scanner emits. The differential net above
% only exercises valid fixtures, so this closes the other half of parity: the
% native parser (now the sole authority for classdef acceptance) must not accept
% what the scanner rejects, and must report it identically.
invalid_dir = [modulepath('interpreter', 'tests'), '/classdef_syntax/invalid'];
invalid_files = dir([invalid_dir, '/*.m']);
rejected = 0;
for k = 1:numel(invalid_files)
  source = fileread([invalid_files(k).folder, '/', invalid_files(k).name]);
  native = __classdef_introspect__('native_full_dump', source);
  scanner = __classdef_introspect__('parse_dump', source);
  asserts.istrue(startsWith(native, 'error='));
  asserts.isequal(native, scanner);
  rejected = rejected + 1;
end
asserts.istrue(rejected > 10);
%=============================================================================
