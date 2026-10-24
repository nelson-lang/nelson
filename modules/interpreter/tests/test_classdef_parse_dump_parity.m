%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Parity check for the native classdef front-end.
% __classdef_parse_dump__ renders the parsed classdef skeleton as a
% deterministic, structural string. This test runs it over the whole classdef
% fixture corpus and locks two invariants the current text scanner satisfies
% and a future grammar/AST-based parser must keep satisfying:
%   1. Every real classdef file dumps a well-formed record naming its class.
%   2. The dump is deterministic (identical across repeated parses).
% When the native parser lands, the same dump is asserted equal between the two
% code paths on this corpus, turning this into a differential parity gate.
%=============================================================================
classdef_dir = [modulepath('interpreter', 'tests'), '/classdef'];
files = dir([classdef_dir, '/*.m']);
%=============================================================================
checked = 0;
for k = 1:numel(files)
  filename = [files(k).folder, '/', files(k).name];
  source = fileread(filename);
  % Only classdef files are in scope; helper .m functions are skipped.
  if ~strcmp(parsestring(source), 'classdef')
    continue;
  end
  dump = __classdef_introspect__('parse_dump', source);
  % Well-formed: parsed without error and starts with a class= record.
  asserts.istrue(startsWith(dump, 'class='));
  asserts.isfalse(startsWith(dump, 'error='));
  % The declared class name is the leaf of the file name (Nelson allows the
  % file name to win over the in-source name).
  [~, expectedName] = fileparts(files(k).name);
  firstLine = strtok(dump, sprintf('\n'));
  className = firstLine(numel('class=') + 1:end);
  asserts.isequal(className, expectedName);
  % Deterministic: a second parse of the same source yields the same dump.
  asserts.isequal(__classdef_introspect__('parse_dump', source), dump);
  checked = checked + 1;
end
%=============================================================================
% The corpus must actually contain classdef fixtures, else the loop is a no-op.
asserts.istrue(checked > 10);
%=============================================================================
