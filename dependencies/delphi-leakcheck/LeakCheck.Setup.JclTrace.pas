{***************************************************************************}
{                                                                           }
{           LeakCheck for Delphi                                            }
{                                                                           }
{           Copyright (c) 2015 Honza Rames                                  }
{                                                                           }
{           https://bitbucket.org/shadow_cs/delphi-leakcheck                }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

/// <summary>
///   Unit that if placed as first unit in the project will initialize
///   LeakCheck memory manager together with the JCL raw stack tracer and trace
///   formatter. This configuration has JCL external dependency.
/// </summary>
unit LeakCheck.Setup.JclTrace;

{$I LeakCheck.inc}

interface

uses
  LeakCheck,
  LeakCheck.Utils;

implementation

uses
  LeakCheck.Trace.Jcl;

initialization
{$IFDEF CPUX64}
  TLeakCheck.GetStackTraceProc := JclFramesStackTrace;
{$ELSE}
  TLeakCheck.GetStackTraceProc := JclRawStackTrace;
{$ENDIF}
  TLeakCheck.GetStackTraceFormatterProc := JclStackTraceFormatter;

end.
