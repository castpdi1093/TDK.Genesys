Imports System.Text
Imports HSI
Imports HSI.Config
Imports HSI.DeviceMethods
Imports HSI.Globals
Imports HSI.Timers


' **********************************************************************************************************
'
' Copyright 2015 Faebian Bastiman. All rights reserved.
'
' **********************************************************************************************************
'
' Namespace: GP
'
' Module: GP307
'
' Purpose: Hardware driver for GP 307 ion gauge controller using #XXRD for read
'
' Current version: 1.0.1
'
' Version History
'
' ----------------------------------------------------------------------------------------------------------
' Date          Name                Version     Remarks
' ----------------------------------------------------------------------------------------------------------
' 09/11/2018    Carsten Stemmler   1.0.0       Initial version
'
' ----------------------------------------------------------------------------------------------------------
'
' **********************************************************************************************************
Public Class Genesys

#Region "Initialisation" 'This region contains device initialisation functions and subs

    Property Registry As Dictionary(Of String, DeviceRegister) 'A dictionary that holds the device Registry

    Sub New()

        Registry = New Dictionary(Of String, DeviceRegister)
        With Registry
            'DeviceRegister(Key, Register, Long Name/Description, Variable Type, ReadOnly)
            .Add("I_MV", New DeviceRegister("I_MV", "I_MV", "Current Measured Value", GetType(Single), True))
            .Add("V_MV", New DeviceRegister("V_MV", "V_MV", "Voltage Measured Value", GetType(Single), True))
            .Add("ADR", New DeviceRegister("ADR", "ADR", "Adresse of Rs232 communication default 6", GetType(String), True))
            '.Add("IG_F2", New DeviceRegister("IG_F2", "IG2", "Filament 2 State", GetType(OnOff), True))
            '.Add("IG_DG", New DeviceRegister("IG_DG", "IGDG", "Degas State", GetType(OnOff), True))
        End With

    End Sub

    'Add a client's cache register to the regualr read list
    Public Sub CreateRegularReads(ClientID As Byte)

        With Config.Clients(ClientID)
            AddOrUpdate_List(.RegularRead, "I_MV") 'Read IG register value
            AddOrUpdate_List(.RegularRead, "V_MV") 'Read CG register value

        End With

    End Sub

    Public Sub InitialiseDevice(DeviceID As String)
        Dim s As String = "ADR 06"
        Dim ClientID As Byte = Config.CacheFetch.GetClientID(DeviceID)
        Dim SA As Byte = Config.CacheFetch.GetSA(DeviceID)
        Config.CacheFetch.AddToWriteQueue(ClientID, SA, "ADR", 0, False)




    End Sub

#End Region

#Region "Read and Write" 'This region contains function related to read and write commands

    'Processes the read queue depending on the device's communication method
    Public Function ProcessReadQueue(ClientID As Byte) As Config.ClientConfig_Class.ReadWriteItem

        Return HSI.ProcessReadQueue_FIRST(ClientID)
        Return Nothing
    End Function

    'Makes a READ string to request values from the device
    Public Function MakeReadString(RWI As Config.ClientConfig_Class.ReadWriteItem) As String

        Dim s As String = ""

        Select Case RWI.Reg
            Case "I_MV" : s = "DVC?"
            Case "V_MV" : s = "DVC?"
            Case "ADR" : s = "ADR 06"
        End Select




        'Select Case RWI.Reg
        '    Case "IG_MV" : s = "#" & sSA(0) & sSA(1) & "RD"
        '        'Case "IG1" : s = "DSIG1"
        '        'Case "IG2" : s = "DSIG2"
        '        'Case "IGDG" : s = "DGS"
        'End Select

        s += Chr(13) & Chr(10) 'msg & (carriage return) & (line feed)
        Return s

    End Function

    'Makes a WRITE string to set values on the device
    Public Function MakeWriteString(RWI As Config.ClientConfig_Class.ReadWriteItem) As String

        Dim s As String = ""
        's = RWI.Reg
        'Select Case RWI.Value
        '    Case 0 : s += " OFF "
        '    Case 1 : s += " ON "
        'End Select
        's += Chr(13) & Chr(10)
        Return s

    End Function

    'Process messages sent from the device (can be in response to either READ or WRITE requests)
    Public Function ProcessResponse(ClientID As Byte, ByRef SA As Byte, sSend As String, sReply As String) As Config.WritePair()
        Dim NWL As New List(Of Config.WritePair)
        Dim answer As String

        If MessageOK(sSend, sReply) Then 'if the messages are ok

            'Dim bSend() As Byte = Encoding.Default.GetBytes(sSend.ToCharArray) 'check message: DEBUG only
            'Dim bReply() As Byte = Encoding.Default.GetBytes(sReply.ToCharArray) 'check message: DEBUG only


            SA = 1 'Set SA
            If Not Config.CacheFetch.GetFound(ClientID, SA) Then Config.CacheFetch.SetFound(ClientID, SA)
            'Is it a read or write message?
            Dim Read As Boolean = True
            'If Not sSend.StartsWith("D") Then Read = False 'some test for write

            answer = sReply
            Select Case True
                Case Read
                    Select Case sSend.Substring(0, sSend.Length - 2)
                        Case "DVC?"
                            sReply = answer.Substring(0, 6)
                            Dim sng As Single = Val(sReply)
                            NWL.Add(New Config.WritePair("V_MV", sng)) 'added >0 check.
                            sReply = answer.Substring(14, 6)
                            Dim sng1 As Single = Val(sReply)
                            NWL.Add(New Config.WritePair("I_MV", sng)) 'added >0 check.






                    End Select



                Case Not Read
                    'Dim sVal As String = sSend.Substring(0, 3)
                    'Dim bVal As Byte = 0
                    'If sSend.Contains("ON") Then bVal = 1
                    'NWL.Add(New Config.WritePair(sVal, bVal))

                    'Currently there are no writes



            End Select
            Return NWL.ToArray
        End If
        Return Nothing


    End Function

    'Returns true if the meassages are in the expected format
    Private Function MessageOK(sSend As String, sReply As String) As Boolean

        If Not String.IsNullOrEmpty(sSend) AndAlso Not String.IsNullOrEmpty(sReply) Then
            If sReply.Length > 15 Then

                Return True

            End If
            'If sSend.StartsWith("DGS") AndAlso sReply.Length = 3 Then Return True
            'If sReply.StartsWith("OK") Then Return True
        End If
        Return False

    End Function

#End Region

#Region "Simulator" 'This region contains functions related to emulating the device in simulation mode

    'SIM ONLY: Initialises a client's cache with default values
    Public Sub CreateCache_Sim(ClientID As Byte, SA As Byte)

        Dim LoopName As String = Config.CacheFetch.GetDeviceID(ClientID, SA).Split(".")(0)
        Dim LoopData = Config.Loops(LoopName) 'DirectCast(Config.Loops(LoopName), Config.LoopConfig_Class)
        Config.CacheFetch.SetData(ClientID, SA, "I_MV", CSng(LoopData.MV_OK_Lo * 10), True) 'Set IG MV to appropriate value
        Config.CacheFetch.SetData(ClientID, SA, "V_MV", CSng(LoopData.MV_OK_Lo * 10), True) 'Set IG MV to appropriate value
        Config.CacheFetch.SetData(ClientID, SA, "IG_F1", CByte(1), True) 'Set IG F1 to 1 = On
        Config.CacheFetch.SetData(ClientID, SA, "IG_F2", CByte(0), True) 'Set IG F2 to 0 = Off
        Config.CacheFetch.SetData(ClientID, SA, "IG_DG", CByte(0), True) 'Set IG DG to 0 = Off

    End Sub

    'SIM ONLY: Processes messages sent to the device from EPIC and returns an array used to update the client's cache
    Public Function ProcessRequest_Sim(ByRef SA As Byte, Msg As String) As Config.WritePair()

        Dim bytes() As Byte = Encoding.Default.GetBytes(Msg.ToCharArray) 'check message: DEBUG only
        Dim OK As Boolean = True

        If OK Then
            Dim NWL As New List(Of Config.WritePair)

            SA = 1
            Dim Read As Boolean = True
            'If Not Msg.StartsWith("D") Then Read = False

            Select Case True
                Case Read
                    NWL.Add(New Config.WritePair("I_MV", Nothing))
                    NWL.Add(New Config.WritePair("V_MV", Nothing))
                Case Not Read
                    'Dim sVal As String = Msg.Substring(0, 3)
                    'Dim bVal As Byte = 0
                    'If Msg.Contains("ON") Then bVal = 1
                    'NWL.Add(New Config.WritePair(sVal, bVal))

            End Select


            Return NWL.ToArray
        End If
        Return Nothing

    End Function

    'SIM ONLY: Wrapper function that devices whether the message is a READ or WRITE command and calls the appropriate function
    Public Function CreateMessage_Sim(ClientID As Byte, SA As Byte, WP() As Config.WritePair) As String

        If WP IsNot Nothing AndAlso Not WP.Length = 0 Then
            If WP(0).Value Is Nothing Then 'read
                Return MakeReadResponse_Sim(ClientID, SA, WP)
            Else
                Return MakeWriteResponse_Sim(ClientID, SA, WP)
            End If
        End If
        Return Nothing

    End Function

    'SIM ONLY: Creates a response to a READ command sent from EPIC to the device
    Public Function MakeReadResponse_Sim(ClientID As Byte, SA As Byte, WP() As Config.WritePair) As String

        Dim s As String = "* "

        Dim sng As Single
        Dim sVal As String = ""
        Select Case WP(0).Key
            Case "IG_MV"
                If Config.CacheFetch.GetData(ClientID, SA, WP(0).Key & "1", False) = 0 AndAlso Config.CacheFetch.GetData(ClientID, SA, WP(0).Key & "2", False) = 0 Then
                    sng = 9900000000.0
                Else
                    If Config.Clients(ClientID).Caches(SA).Cache("IG_MV").Value > 100000000.0 Then
                        Dim LoopName As String = Config.CacheFetch.GetDeviceID(ClientID, SA).Split(".")(0)
                        Dim LoopData = Config.Loops(LoopName) 'DirectCast(Config.Loops(LoopName), Config.LoopConfig_Class)
                        sng = LoopData.MV_OK_Lo * 10
                    Else
                        sng = Config.Clients(ClientID).Caches(SA).Cache("IG_MV").Value
                    End If

                End If


                sVal = FormatDouble(sng, False, "0.00E+00")
                'Case "IG1", "IG2"
                '    'Dim sNum As String = WP(0).Key.Substring(2, 1)
                '    If Config.CacheFetch.GetData(ClientID, SA, WP(0).Key, False) = 0 Then
                '        sng = 9900000000.0
                '    Else
                '        sng = Config.Clients(ClientID).Caches(SA).Cache("IG_MV").Value
                '    End If
                '    sVal = FormatDouble(sng, False, "0.00E+0")
                'Case "IGDG"
                '    sng = Config.CacheFetch.GetData(ClientID, SA, WP(0).Key, False)
                '    sVal = FormatDouble(sng, False, "0")
        End Select




        s += sVal & Chr(13)

        Return s

    End Function

    'SIM ONLY: Creates a response to a WRTIE command sent from EPIC to the device
    Public Function MakeWriteResponse_Sim(ClientID As Byte, SA As Byte, WPs() As Config.WritePair) As String

        Dim s As String = "OK" & Chr(13) & Chr(10)
        Return s

    End Function

    'SIM ONLY: Performs device specific random number generation to give the simulator new values to display
    Public Sub Update_Sim(ClientID As Byte)

        For Each Cache In Config.Clients(ClientID).Caches
            SyncLock Cache.Value.Cache_Lock
                Cache.Value.Cache("I_MV").Value = 0.000000082
                Cache.Value.Cache("V_MV").Value = 0.000000082
                'If Cache.Value.Cache("IG_MV").Value < 100000000.0 Then Cache.Value.Cache("IG_MV").Value = RandomPressure(Cache.Value.Cache("IG_MV").Value)
            End SyncLock
        Next

    End Sub

#End Region

#Region "Other" 'This region contains device specific function and subs







#End Region


End Class



