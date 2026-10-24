// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Text;

public static class NelsonConsoleLaunch
{
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    private struct StartupInfo
    {
        public uint Size;
        public string Reserved, Desktop, Title;
        public uint X, Y, Width, Height, Columns, Rows, Fill, Flags;
        public ushort ShowWindow, ReservedSize;
        public IntPtr ReservedPointer, Input, Output, Error;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct ProcessInfo
    {
        public IntPtr Process, Thread;
        public uint ProcessId, ThreadId;
    }

    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern bool CreateProcessW(string application, StringBuilder command,
        IntPtr processAttributes, IntPtr threadAttributes, bool inherit, uint flags,
        IntPtr environment, string directory, ref StartupInfo startup, out ProcessInfo process);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern uint WaitForSingleObject(IntPtr handle, uint timeout);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool GetExitCodeProcess(IntPtr process, out uint code);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool TerminateProcess(IntPtr process, uint code);
    [DllImport("kernel32.dll")]
    private static extern bool CloseHandle(IntPtr handle);

    public static uint Run(string executable, string destination, string directory)
    {
        // These two absolute filenames cannot contain quotes or end in a separator.
        var command = new StringBuilder("\"" + executable + "\" \"" + destination + "\" probe");
        var startup = new StartupInfo { Size = (uint)Marshal.SizeOf<StartupInfo>(), Flags = 1, ShowWindow = 0 };
        ProcessInfo process;
        // A hidden NEW_CONSOLE is explicit; no inherited no-window state is involved.
        if (!CreateProcessW(executable, command, IntPtr.Zero, IntPtr.Zero, false,
                0x10, IntPtr.Zero, directory, ref startup, out process))
            throw new Win32Exception(Marshal.GetLastWin32Error());
        try
        {
            uint wait = WaitForSingleObject(process.Process, 30000);
            if (wait != 0)
            {
                TerminateProcess(process.Process, 99);
                WaitForSingleObject(process.Process, 30000);
                throw new InvalidOperationException("Native console control timed out or failed.");
            }
            uint code;
            if (!GetExitCodeProcess(process.Process, out code))
                throw new Win32Exception(Marshal.GetLastWin32Error());
            return code;
        }
        finally
        {
            CloseHandle(process.Thread);
            CloseHandle(process.Process);
        }
    }
}
