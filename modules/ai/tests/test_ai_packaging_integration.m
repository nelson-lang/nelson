%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
root = nelsonroot();
%=============================================================================
% This suite validates the installer/build configuration (*.iss, *.vcxproj,
% nelson.sln), which exists only in the source tree. Installed builds do not
% ship these files, so skip there.
skip_testsuite(~isfile([root, '/modules/modules.iss']), ...
  'Packaging integration test requires the Nelson source tree.');
%=============================================================================
modules_iss = fileread([root, '/modules/modules.iss']);
asserts.istrue(contains(modules_iss, '#include "ai/module.iss"'));
%=============================================================================
ai_iss = fileread([root, '/modules/ai/module.iss']);
asserts.istrue(contains(ai_iss, 'libnlsAi.dll'));
asserts.istrue(contains(ai_iss, 'libnlsAi_builtin.dll'));
asserts.istrue(contains(ai_iss, 'Components: {#COMPONENT_AI}'));
asserts.isfalse(contains(ai_iss, 'COMPONENT_WEBTOOLS'));
asserts.istrue(contains(ai_iss, 'modules\{#MODULE_NAME}\tests\*.m'));
asserts.istrue(contains(ai_iss, 'modules\{#MODULE_NAME}\skills-catalog\*'));
%=============================================================================
components_iss = fileread([root, '/tools/innosetup/components.iss']);
asserts.istrue(contains(components_iss, '#define COMPONENT_AI'));
asserts.istrue(contains(components_iss, 'Description:{cm:AI_MODULE}'));
%=============================================================================
code_iss = fileread([root, '/tools/innosetup/code.iss']);
asserts.istrue(contains(code_iss, 'ModulesList.Add(ExpandConstant(''{#COMPONENT_AI}''))'));
asserts.istrue(contains(code_iss, 'EnsureDependency(''{#COMPONENT_JSON}'', ''{#COMPONENT_AI}'''));
%=============================================================================
english_messages = fileread([root, '/tools/innosetup/english_messages_nelson.islu']);
french_messages = fileread([root, '/tools/innosetup/french_messages_nelson.islu']);
asserts.istrue(contains(english_messages, 'english.AI_MODULE='));
asserts.istrue(contains(french_messages, 'french.AI_MODULE='));
%=============================================================================
solution = fileread([root, '/nelson.sln']);
asserts.istrue(contains(solution, '"nlsAi"'));
asserts.istrue(contains(solution, '"nlsAi_builtin"'));
asserts.istrue(contains(solution, 'modules\ai\src\c\nlsAi.vcxproj'));
asserts.istrue(contains(solution, 'modules\ai\builtin\c\nlsAi_builtin.vcxproj'));
asserts.istrue(contains(solution, '{69A15CE5-B58C-4AF2-8238-0853661CB7F6}'));
asserts.istrue(contains(solution, '{1214F0C1-92AA-4C60-8C2E-605A645A4AB4}'));
%=============================================================================
src_project = fileread([root, '/modules/ai/src/c/nlsAi.vcxproj']);
asserts.istrue(contains(src_project, '<ProjectGuid>{69A15CE5-B58C-4AF2-8238-0853661CB7F6}</ProjectGuid>'));
asserts.istrue(contains(src_project, 'NLSAI_EXPORTS'));
asserts.istrue(contains(src_project, 'AiMcpServer.cpp'));
asserts.istrue(contains(src_project, 'AiMcpServerQualityTools.cpp'));
asserts.istrue(contains(src_project, 'AiMcpServerTools.cpp'));
asserts.istrue(contains(src_project, 'nlsJson.vcxproj'));
%=============================================================================
builtin_project = fileread([root, '/modules/ai/builtin/c/nlsAi_builtin.vcxproj']);
asserts.istrue(contains(builtin_project, '<ProjectGuid>{1214F0C1-92AA-4C60-8C2E-605A645A4AB4}</ProjectGuid>'));
asserts.istrue(contains(builtin_project, 'NLSAI_BUILTIN_EXPORTS'));
asserts.istrue(contains(builtin_project, 'Gateway.cpp'));
asserts.istrue(contains(builtin_project, 'mcpinfoBuiltin.cpp'));
asserts.istrue(contains(builtin_project, 'mcpserverBuiltin.cpp'));
asserts.istrue(contains(builtin_project, '..\..\src\c\nlsAi.vcxproj'));
%=============================================================================
