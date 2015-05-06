<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Web Tools\DNX\dnx-clr-win-x64.1.0.0-beta4\bin\Microsoft.CodeAnalysis.CSharp.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Web Tools\DNX\dnx-clr-win-x64.1.0.0-beta4\bin\Microsoft.CodeAnalysis.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Text.Encoding.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Threading.Tasks.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <GACReference>System.Collections.Immutable, Version=1.1.33.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

let sample = """using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using PracticeManagement.Foundation.Helpers;
using PracticeManagement.Foundation.MVVM;
using Telerik.Windows.Controls;
using Telerik.Windows.Controls.ScheduleView;
using System.Windows.Xps.Packaging;
using System.Windows.Media;
using System.Windows;

namespace PracticeManagement.Foundation.DataModels
{
    public class AppointmentDataModel : DataModelBase, IAppointment
    {
        #region Patient - PatientDataModel
        private PatientDataModel _Patient;

        public PatientDataModel Patient
        {
            get
            {
                if (_Patient == null)
                    Patient = new PatientDataModel();
                return _Patient;
            }

            set
            {
                var oldPatient = _Patient;
                if (this.SetAndNotify(() => this.Patient, ref _Patient, value))
                {
                    if (oldPatient != null)
                        oldPatient.PropertyChanged -= Patient_PropertyChanged;
                    if (_Patient != null)
                    {
                        _Patient.PropertyChanged += Patient_PropertyChanged;
                        Subject = _Patient.LastNameFirstName;
                    }
                    else
                        Subject = string.Empty;
                }
            }
        }

        private void Patient_PropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            switch (e.PropertyName)
            {
                case "LastNameFirstName":
                    this.Subject = Patient.LastNameFirstName;
                    break;
                default:
                    break;
            }
        }
        #endregion

        #region AppointmentGuarantorProfileID - int
        private int _AppointmentGuarantorProfileID = 0;

        public int AppointmentGuarantorProfileID
        {
            get { return _AppointmentGuarantorProfileID; }
            set { this.SetAndNotify(() => this.AppointmentGuarantorProfileID, ref _AppointmentGuarantorProfileID, value); }
        }
        #endregion

        #region AppointmentPatientID - int
        private int _AppointmentPatientID = 0;

        public int AppointmentPatientID
        {
            get { return _AppointmentPatientID; }
            set { this.SetAndNotify(() => this.AppointmentPatientID, ref _AppointmentPatientID, value); }
        }
        #endregion

        #region AppointmentGuarantorProfileInfoID - int
        private int _AppointmentGuarantorProfileInfoID = 0;

        public int AppointmentGuarantorProfileInfoID
        {
            get { return _AppointmentGuarantorProfileInfoID; }
            set { this.SetAndNotify(() => this.AppointmentGuarantorProfileInfoID, ref _AppointmentGuarantorProfileInfoID, value); }
        }
        #endregion

        #region AppointmentTypeID - int
        private int _AppointmentTypeID = 0;

        public int AppointmentTypeID
        {
            get { return _AppointmentTypeID; }
            set { this.SetAndNotify(() => this.AppointmentTypeID, ref _AppointmentTypeID, value); }
        }
        #endregion

        #region AppointmentID - int
        private int _AppointmentID = 0;
        public int AppointmentID
        {
            get { return _AppointmentID; }
            set { this.SetAndNotify(() => this.AppointmentID, ref _AppointmentID, value); }
        }
        #endregion

        #region CurrentPayerID - int
        private int _CurrentPayerID = 0;
        public int CurrentPayerID
        {
            get { return _CurrentPayerID; }
            set { this.SetAndNotify(() => this.CurrentPayerID, ref _CurrentPayerID, value); }
        }
        #endregion

        #region AppointmentProviderScheduledID - int
        private int _AppointmentProviderScheduledID = 0;
        public int AppointmentProviderScheduledID
        {
            get { return _AppointmentProviderScheduledID; }
            set { this.SetAndNotify(() => this.AppointmentProviderScheduledID, ref _AppointmentProviderScheduledID, value); }
        }
        #endregion

        #region AppointmentFacilityID - int
        private int _AppointmentFacilityID = (SessionVariables.Instance.CurrentFacility == null) ? 0 : SessionVariables.Instance.CurrentFacility.FacilityID;

        public int AppointmentFacilityID
        {
            get { return _AppointmentFacilityID; }
            set { this.SetAndNotify(() => this.AppointmentFacilityID, ref _AppointmentFacilityID, value); }
        }
        #endregion

        #region AppointmentFaciltyStartTime - TimeSpan
        public TimeSpan AppointmentFaciltyStartTime
        {
            get
            {
                if (SessionVariables.Instance.CurrentFacility != null)
                    return SessionVariables.Instance.CurrentFacility.FacilityOpeningTime;

                return new TimeSpan(0, 0, 0);
            }
        }
        #endregion

        #region AppointmentStartTime - DateTime
        //Next closest 15 minute increment
        private DateTime _AppointmentStartTime = DateTime.Now.AddMinutes((DateTime.Now.Minute % 15 == 0 ? 0 : 15) - (DateTime.Now.Minute % 15));
        public DateTime AppointmentStartTime
        {
            get { return _AppointmentStartTime; }
            set { this.SetAndNotify(() => this.AppointmentStartTime, ref _AppointmentStartTime, value); }
        }
        #endregion

       
        #region AppointmentSlotDateTime - DateTime?
        private DateTime? _AppointmentSlotDateTime = null;

        public DateTime? AppointmentSlotDateTime
        {
            get { return _AppointmentSlotDateTime; }
            set { this.SetAndNotify(() => this.AppointmentSlotDateTime, ref _AppointmentSlotDateTime, value); }
        }
        #endregion

        #region AppointmentEndTime - DateTime
        //Half hour from the next closest 15 minute increment
        private DateTime _AppointmentEndTime = DateTime.Now.AddMinutes((DateTime.Now.Minute % 15 == 0 ? 0 : 15) - (DateTime.Now.Minute % 15)).AddMinutes(30);
        public DateTime AppointmentEndTime
        {
            get { return _AppointmentEndTime; }
            set { this.SetAndNotify(() => this.AppointmentEndTime, ref _AppointmentEndTime, value); }
        }
        #endregion

        #region AppointmentStartDate - string
        //Half hour from the next closest 15 minute increment
        private string _AppointmentStartDate = string.Empty;
        public string AppointmentStartDate
        {
            get
            {
                if (this.Start != null)
                {
                    _AppointmentStartDate = this.Start.ToShortDateString();
                }
                else
                {
                    _AppointmentStartDate = "0/0/0000";
                }

                return _AppointmentStartDate;
            }

        }
        #endregion

        //#region AppointmentFaciltyStartTime - TimeSpan
        //public TimeSpan AppointmentFaciltyStartTime
        //{
        //    get
        //    {
        //        if (SessionVariables.Instance.CurrentFacility != null)
        //            return SessionVariables.Instance.CurrentFacility.FacilityOpeningTime;

        //        return new TimeSpan(0, 0, 0);
        //    }
        //}
        //#endregion

        #region AppointmentFacilityEndTime - TimeSpan
        public TimeSpan AppointmentFacilityEndTime
        {
            get
            {
                if (SessionVariables.Instance.CurrentFacility != null)
                    return SessionVariables.Instance.CurrentFacility.FacilityClosingTime;

                return new TimeSpan(11, 59, 0);
            }
        }
        #endregion

        #region AppointmentStatus - string
        private string _AppointmentStatus = "Scheduled";
        public string AppointmentStatus
        {
            get { return _AppointmentStatus; }
            set
            {
                if (this.SetAndNotify(() => this.AppointmentStatus, ref _AppointmentStatus, value))
                {
                    switch (value)
                    {
                        case "Checked In":
                            LinearGradientBrush checkIn = new LinearGradientBrush();
                            checkIn.StartPoint = new Point(0.5, 0);
                            checkIn.EndPoint = new Point(0.5, 1);
                            GradientStop lightGreen = new GradientStop((Color)ColorConverter.ConvertFromString("#EEFFEE"), 0.33);
                            GradientStop middleGreen = new GradientStop((Color)ColorConverter.ConvertFromString("#D8FFD8"), 0.66);
                            GradientStop green = new GradientStop((Color)ColorConverter.ConvertFromString("#C3FFC3"), 1);
                            checkIn.GradientStops.Add(lightGreen);
                            checkIn.GradientStops.Add(middleGreen);
                            checkIn.GradientStops.Add(green);
                            AppointmentItemLinearGradientBrush = checkIn;
                            IsNoCheckedOutAppointment = true;
                            break;
                        case "Checked Out":
                            LinearGradientBrush checkOut = new LinearGradientBrush();
                            checkOut.StartPoint = new Point(0.5, 0);
                            checkOut.EndPoint = new Point(0.5, 1);
                            GradientStop lightGray = new GradientStop((Color)ColorConverter.ConvertFromString("#DEDEDE"), 0.33);
                            GradientStop middleGray = new GradientStop((Color)ColorConverter.ConvertFromString("#CECECE"), 0.66);
                            GradientStop gray = new GradientStop((Color)ColorConverter.ConvertFromString("#C0C0C0"), 1);
                            checkOut.GradientStops.Add(lightGray);
                            checkOut.GradientStops.Add(middleGray);
                            checkOut.GradientStops.Add(gray);
                            AppointmentItemLinearGradientBrush = checkOut;
                            IsNoCheckedOutAppointment = false;
                            break;
                        default:
                            LinearGradientBrush normal = new LinearGradientBrush();
                            normal.StartPoint = new Point(0.5, 0);
                            normal.EndPoint = new Point(0.5, 1);
                            GradientStop white = new GradientStop(Colors.White, 0.50);
                            GradientStop whiteSmoke = new GradientStop(Colors.WhiteSmoke, 1);
                            normal.GradientStops.Add(white);
                            normal.GradientStops.Add(whiteSmoke);
                            AppointmentItemLinearGradientBrush = normal;
                            IsNoCheckedOutAppointment = true;
                            break;
                    }
                }
            }
        }
        #endregion

        #region AppointmentCheckInFlag - bool
        private bool _AppointmentCheckInFlag = false;

        public bool AppointmentCheckInFlag
        {
            get { return _AppointmentCheckInFlag; }
            set { this.SetAndNotify(() => this.AppointmentCheckInFlag, ref _AppointmentCheckInFlag, value); }
        }
        #endregion

        #region AppointmentCheckInTime - DateTime?
        private DateTime? _AppointmentCheckInTime = null;
        public DateTime? AppointmentCheckInTime
        {
            get { return _AppointmentCheckInTime; }
            set { this.SetAndNotify(() => this.AppointmentCheckInTime, ref _AppointmentCheckInTime, value); }
        }
        #endregion

        #region AppointmentCheckOutTime - DateTime?
        private DateTime? _AppointmentCheckOutTime = null;
        public DateTime? AppointmentCheckOutTime
        {
            get { return _AppointmentCheckOutTime; }
            set { this.SetAndNotify(() => this.AppointmentCheckOutTime, ref _AppointmentCheckOutTime, value); }
        }
        #endregion

        #region AppointmentBillingStage - int
        private int _AppointmentBillingStage = 0;

        public int AppointmentBillingStage
        {
            get { return _AppointmentBillingStage; }
            set { this.SetAndNotify(() => this.AppointmentBillingStage, ref _AppointmentBillingStage, value); }
        }
        #endregion

        #region AppointmentBillingStageText - string
        private string _AppointmentBillingStageText = string.Empty;

        public string AppointmentBillingStageText
        {
            get
            {
                switch (AppointmentBillingStage)
                {
                    case 1: return "Code Chart"; break;
                    case 2: return "Prep Claims"; break;
                    case 3: return "Reconcile Payments"; break;
                    case 4: return "Submitted/Waiting"; break;
                    case 5: return "Claim Review"; break;
                    case 6: return "Pt. Billing"; break;
                    case 7: return "Patient A/R"; break;
                    case 8: return "Completed"; break;
                    case 9: return "Refunds"; break;
                    default: return "Unknown"; break;
                }
            }

            set { this.SetAndNotify(() => this.AppointmentBillingStageText, ref _AppointmentBillingStageText, value); }
        }
        #endregion

        #region AppointmentXPressChart - XpsDocument
        private XpsDocument _AppointmentXPressChart;

        public XpsDocument AppointmentXPressChart
        {
            get { return _AppointmentXPressChart; }
            set { this.SetAndNotify(() => this.AppointmentXPressChart, ref _AppointmentXPressChart, value); }
        }
        #endregion

        #region AppointmentForeignEHRID - int?
        private int? _AppointmentForeignEHRID;

        public int? AppointmentForeignEHRID
        {
            get { return _AppointmentForeignEHRID; }
            set { this.SetAndNotify(() => this.AppointmentForeignEHRID, ref _AppointmentForeignEHRID, value); }
        }
        #endregion

        #region AppointmentAccidentRelated - bool
        private bool _AppointmentAccidentRelated = false;

        public bool AppointmentAccidentRelated
        {
            get { return _AppointmentAccidentRelated; }
            set
            {
                if (this.SetAndNotify(() => this.AppointmentAccidentRelated, ref _AppointmentAccidentRelated, value))
                    if (!value)
                    {
                        AppointmentAccidentID = 0;
                        AppointmentAccidentDate = null;
                    }
            }
        }
        #endregion

        #region AppointmentAccidentID - int
        private int _AppointmentAccidentID = 0;

        public int AppointmentAccidentID
        {
            get { return _AppointmentAccidentID; }
            set { this.SetAndNotify(() => this.AppointmentAccidentID, ref _AppointmentAccidentID, value); }
        }
        #endregion

        #region AppointmentAccidentDate - DateTime?
        private DateTime? _AppointmentAccidentDate = null;

        public DateTime? AppointmentAccidentDate
        {
            get { return _AppointmentAccidentDate; }
            set { this.SetAndNotify(() => this.AppointmentAccidentDate, ref _AppointmentAccidentDate, value); }
        }
        #endregion

        #region AppointmentAccidentState - string
        private string _AppointmentAccidentState = string.Empty;

        public string AppointmentAccidentState
        {
            get { return _AppointmentAccidentState; }
            set { this.SetAndNotify(() => this.AppointmentAccidentState, ref _AppointmentAccidentState, value); }
        }
        #endregion

        #region AppointmentAccidentTypeString - string
        private string _AppointmentAccidentTypeString = string.Empty;

        public string AppointmentAccidentTypeString
        {
            get { return _AppointmentAccidentTypeString; }
            set { this.SetAndNotify(() => this.AppointmentAccidentTypeString, ref _AppointmentAccidentTypeString, value); }
        }
        #endregion

        #region ChartExists - bool
        public bool ChartExists
        {
            get { return AppointmentStatus == "Checked Out"; }
        }
        #endregion

        #region AppointmentItemLinearGradientBrush - LinearGradientBrush
        private LinearGradientBrush _AppointmentItemLinearGradientBrush = new LinearGradientBrush(Colors.White, Colors.WhiteSmoke, new Point(0.5, 0), new Point(0.5, 1));

        public LinearGradientBrush AppointmentItemLinearGradientBrush
        {
            get { return _AppointmentItemLinearGradientBrush; }
            set { this.SetAndNotify(() => this.AppointmentItemLinearGradientBrush, ref _AppointmentItemLinearGradientBrush, value); }
        }
        #endregion

        private bool _IsNoCheckedOutAppointment = false;
        public bool IsNoCheckedOutAppointment
        {
            get
            {
                return _IsNoCheckedOutAppointment;
            }
            set
            {
                this.SetAndNotify(() => this.IsNoCheckedOutAppointment, ref _IsNoCheckedOutAppointment, value);
            }
        }
        #region ClaimEnabled - string
        private string _ClaimEnabled = string.Empty;

        public string ClaimEnabled
        {
            get { return _ClaimEnabled; }
            set { this.SetAndNotify(() => this.ClaimEnabled, ref _ClaimEnabled, value); }
        }
        #endregion


        #region Address2 - string
        private string _Address2 = string.Empty;
        public string Address2
        {
            get { return _Address2; }
            set { this.SetAndNotify(() => this.Address2, ref _Address2, value); }
        }
        #endregion

        #region PresentingCondition - string
        private string _PresentingCondition = string.Empty;

        public string PresentingCondition
        {
            get { return _PresentingCondition; }
            set { this.SetAndNotify(() => this.PresentingCondition, ref _PresentingCondition, value); }
        }
        #endregion

        #region NotesToBiller - string
        private string _NotesToBiller = string.Empty;

        public string NotesToBiller
        {
            get { return _NotesToBiller; }
            set { this.SetAndNotify(() => this.NotesToBiller, ref _NotesToBiller, value); }
        }
        #endregion

        #region ReviewReadonly
        private bool _ReviewReadonly = false;
        public bool ReviewReadonly
        {
            get
            {

                return _ReviewReadonly;


            }
            set { this.SetAndNotify(() => this.ReviewReadonly, ref _ReviewReadonly, value); }
        }
        #endregion

        #region LOS - string
        private string _LOS = string.Empty;
        public string LOS
        {
            get
            {
                DateTime first;
                DateTime second;

                switch (AppointmentStatus)
                {
                    case "Checked In":
                        first = Convert.ToDateTime(AppointmentCheckInTime);
                        second = DateTime.Now;
                        break;
                    case "Checked Out":
                        return _LOS;
                    case "Scheduled":
                        return string.Empty;
                    default:
                        return string.Empty;
                }

                TimeSpan los = second - first;

                string minutes;
                if (los.Minutes < 10 && los.Minutes != 0)
                    minutes = "0" + los.Minutes.ToString();
                else
                    minutes = los.Minutes == 0 ? "00" : los.Minutes.ToString();

                return String.Format("{0}:{1}",
                    los.Hours == 0 ? "0" : los.Hours.ToString(),
                    minutes);
            }
            set { this.SetAndNotify(() => this.LOS, ref _LOS, value); }
        }
        #endregion

        #region SelfCharges - decimal
        private decimal _SelfCharges = 0;

        public decimal SelfCharges
        {
            get { return _SelfCharges; }
            set { this.SetAndNotify(() => this.SelfCharges, ref _SelfCharges, value); }
        }
        #endregion

        #region TotalCharges - decimal
        private decimal _TotalCharges = 0;

        public decimal TotalCharges
        {
            get { return _TotalCharges; }
            set { this.SetAndNotify(() => this.TotalCharges, ref _TotalCharges, value); }
        }
        #endregion

        #region TotalPayments - decimal
        private decimal _TotalPayments = 0;

        public decimal TotalPayments
        {
            get { return _TotalPayments; }
            set { this.SetAndNotify(() => this.TotalPayments, ref _TotalPayments, value); }
        }
        #endregion

        #region TotalInsurancePayments - decimal
        private decimal _TotalInsurancePayments = 0;

        public decimal TotalInsurancePayments
        {
            get { return _TotalInsurancePayments; }
            set { this.SetAndNotify(() => this.TotalInsurancePayments, ref _TotalInsurancePayments, value); }
        }
        #endregion

        #region TotalPatientPayments - decimal
        private decimal _TotalPatientPayments = 0;

        public decimal TotalPatientPayments
        {
            get { return _TotalPatientPayments; }
            set { this.SetAndNotify(() => this.TotalPatientPayments, ref _TotalPatientPayments, value); }
        }
        #endregion

        #region TotalOtherPayments - decimal
        private decimal _TotalOtherPayments = 0;

        public decimal TotalOtherPayments
        {
            get { return _TotalOtherPayments; }
            set { this.SetAndNotify(() => this.TotalOtherPayments, ref _TotalOtherPayments, value); }
        }
        #endregion

        #region TotalPaymentStringForClaims - string
        private string _TotalPaymentStringForClaims = string.Empty;

        public string TotalPaymentStringForClaims
        {
            get { return _TotalPaymentStringForClaims; }
            set { this.SetAndNotify(() => this.TotalPaymentStringForClaims, ref _TotalPaymentStringForClaims, value); }
        }
        #endregion

        #region Balance - decimal
        private decimal _Balance = 0;

        public decimal Balance
        {
            get { return _Balance; }
            set { this.SetAndNotify(() => this.Balance, ref _Balance, value); }
        }
        #endregion

        #region IAppointment Properties
        #region Subject - string
        private string _Subject = String.Empty;

        public string Subject
        {
            get { return _Subject; }
            set { this.SetAndNotify(() => this.Subject, ref _Subject, value); }
        }
        #endregion

        #region Start - DateTime
        private DateTime _Start = DateTime.Now.AddMinutes((DateTime.Now.Minute % 15 == 0 ? 0 : 15) - (DateTime.Now.Minute % 15));

        public DateTime Start
        {
            get { return _Start; }
            set { this.SetAndNotify(() => this.Start, ref _Start, value); }
        }
        #endregion

        #region End - DateTime
        private DateTime _End = DateTime.Now.AddMinutes((DateTime.Now.Minute % 15 == 0 ? 0 : 15) - (DateTime.Now.Minute % 15)).AddMinutes(30);

        public DateTime End
        {
            get { return _End; }
            set { this.SetAndNotify(() => this.End, ref _End, value); }
        }
        #endregion

        #region IsAllDayEvent - bool
        private bool _IsAllDayEvent = false;

        public bool IsAllDayEvent
        {
            get { return _IsAllDayEvent; }
            set { this.SetAndNotify(() => this.IsAllDayEvent, ref _IsAllDayEvent, value); }
        }
        #endregion

        #region IsChecked - bool
        private bool _IsChecked = false;

        public bool IsChecked
        {
            get { return _IsChecked; }
            set { this.SetAndNotify(() => this.IsChecked, ref _IsChecked, value); }
        }
        #endregion

        #region RecurrenceRule - IRecurrenceRule
        private IRecurrenceRule _RecurrenceRule;

        public IRecurrenceRule RecurrenceRule
        {
            get { return _RecurrenceRule; }
            set { this.SetAndNotify(() => this.RecurrenceRule, ref _RecurrenceRule, value); }
        }
        #endregion

        public event EventHandler RecurrenceRuleChanged;

        #region Resources - IList
        private IList _Resources = new ArrayList();

        public IList Resources
        {
            get { return _Resources; }
            set { this.SetAndNotify(() => this.Resources, ref _Resources, value); }
        }
        #endregion

        #region TimeZone - TimeZoneInfo
        private TimeZoneInfo _TimeZone;

        public TimeZoneInfo TimeZone
        {
            get { return _TimeZone; }
            set { this.SetAndNotify(() => this.TimeZone, ref _TimeZone, value); }
        }
        #endregion

        public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;

        public void BeginEdit()
        {
            return;
        }

        public void CancelEdit()
        {
            return;
        }

        public void EndEdit()
        {
            return;
        }

        public bool Equals(IAppointment other)
        {
            return true;
        }

        public IAppointment Copy()
        {
            AppointmentDataModel appointment = new AppointmentDataModel();
            appointment.CopyFrom(this);
            return appointment;
        }

        public void CopyFrom(IAppointment other)
        {
            AppointmentDataModel appointment = other as AppointmentDataModel;
            if (appointment != null)
            {
                foreach (Resource r in appointment.Resources)
                    this.AppointmentProviderScheduledID = Convert.ToInt32(r.ResourceName);

                this.Patient = appointment.Patient;
                this.AppointmentGuarantorProfileID = appointment.AppointmentGuarantorProfileID;
                this.AppointmentGuarantorProfileInfoID = appointment.AppointmentGuarantorProfileInfoID;
                this.AppointmentPatientID = appointment.AppointmentPatientID;
                this.AppointmentTypeID = appointment.AppointmentTypeID;
                this.AppointmentID = appointment.AppointmentID;
                this.AppointmentProviderScheduledID = appointment.AppointmentProviderScheduledID;
                this.AppointmentFacilityID = appointment.AppointmentFacilityID;
                this.AppointmentStatus = appointment.AppointmentStatus;
                this.Subject = appointment.Subject;
                this.Start = appointment.Start;
                this.AppointmentCheckInFlag = appointment.AppointmentCheckInFlag;
                this.AppointmentCheckInTime = appointment.AppointmentCheckInTime;
                this.AppointmentCheckOutTime = appointment.AppointmentCheckOutTime;
                this.AppointmentBillingStage = appointment.AppointmentBillingStage;
                this.AppointmentXPressChart = appointment.AppointmentXPressChart;
                this.Start = appointment.Start;
                this.End = appointment.End;
                this.AppointmentAccidentRelated = appointment.AppointmentAccidentRelated;
                this.AppointmentAccidentID = appointment.AppointmentAccidentID;
                this.AppointmentAccidentDate = appointment.AppointmentAccidentDate;
                this.AppointmentAccidentState = appointment.AppointmentAccidentState;
                this.IsAllDayEvent = appointment.IsAllDayEvent;
                this.RecurrenceRule = appointment.RecurrenceRule;
                this.Resources = appointment.Resources;
                this.TimeZone = appointment.TimeZone;
                this.PresentingCondition = appointment.PresentingCondition;
                this.NotesToBiller = appointment.NotesToBiller;
                this.IsChecked = appointment.IsChecked;
                if (appointment.AppointmentForeignEHRID != null)
                    this.AppointmentForeignEHRID = appointment.AppointmentForeignEHRID;
            }
        }

        public void Delete()
        {
            return;
        }
        #endregion

      

        #region IsReadOnlyView
        //private bool _isReadOnlyView = false;
        public bool isReadOnlyView
        {
            get
            {
                return (this.AppointmentBillingStage != 4 && this.AppointmentBillingStage != 5);
            }
        }
        #endregion

      

        #region IsDeniedClaim
        public Visibility IsSubWaitClaim
        {
            get
            {
                return (this.AppointmentBillingStage == 4 )? Visibility.Visible: Visibility.Collapsed;
            }
        }
        #endregion

        #region IsVisiblityClaim
        private bool _IsCompleteReview = false;
        public bool IsCompleteReview
        {
            get
            {
                return (this.AppointmentBillingStage != 5);
            }
        }
        #endregion

        #region IsClaimReview
        public Visibility IsClaimReview
        {
            get
            {
                return (this.AppointmentBillingStage == 5) ? Visibility.Visible : Visibility.Collapsed;
               
            }
        }
        #endregion



        #region Place of Service ID - int
        private int _PlaceofServiceID = 0;

        public int PlaceofServiceID
        {
            get { return _PlaceofServiceID; }
            set { this.SetAndNotify(() => this.PlaceofServiceID, ref _PlaceofServiceID, value); }
        }
        #endregion


    }
}
"""

open System
open System.Collections.Generic
open System.IO

module ScriptOptions = 
    let promoteUninitializedStructsToNullable = true
    let includeOriginalInComments = true
    let includeMatchTypeInComments = true
    let spacing = "  "
    let selfIdentifier = "x"
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
let inline dumps s o = printfn "%s:%A" s o; o
let inline flip f arg1 arg2 = f arg2 arg1
type System.String with
    static member before (d:string) (s:string) = s.Substring(0, s.IndexOf(d))
    static member after (d:string) (s:string) = s.Substring( s.IndexOf(d) + d.Length)
    static member contains d (s:string) = s.Contains(d)
    static member beforeOrSelf d (s:string) = if s |> String.contains d then s|> String.before d else s
    static member trim (s:string) = s.Trim()
    static member join d (items:string seq) = String.Join(d,items |> Array.ofSeq)
    static member replace (d:string) r (s:string)= s.Replace(d,r)

module Seq =
    let inline iterDump items = Seq.iter (fun i-> printfn "%A" i) items
    let inline dump items = printfn "%A" items; items
    let inline dumps title items = 
        if Seq.isEmpty items then printfn"%s: is empty" title else printfn "%s:%A" title items
        items

    let inline ofType<'a> (source : System.Collections.IEnumerable) : 'a seq= 
        let resultType = typeof<'a>
        seq {
            for item in source do 
                match item with
                | null -> ()
                | _ -> 
                    if resultType.IsAssignableFrom (item.GetType()) then
                        yield (downcast item)
        }
        
    // alternate implementation
    //let ofType<'a> (items: _ seq) = items |> Seq.filter(fun x -> box x :? 'a) |> Seq.cast<'a>
[<AutoOpen>]
module Helpers =
    let enumerateAllFiles rootPath pattern = System.IO.Directory.EnumerateFiles(rootPath,pattern, SearchOption.AllDirectories)
    let readAllText filePath = File.ReadAllText(filePath)


type ModelCollector() = 
    inherit CSharpSyntaxWalker()
    let implementedInterfaces = new Dictionary<string,string list>()
    member private x.ImplementedInterfaces() = implementedInterfaces
    //full of fail:
    // member private x.ImplementedInterfaces = new Dictionary<string,string list>()
    static member VisitClassInterfaces (root:CompilationUnitSyntax) =
        let mc = new ModelCollector()
        mc.Visit(root)
        mc.ImplementedInterfaces()
        //|> Seq.dumps "implemented interfaces"
    override x.VisitBaseList node = 
        let parentIdentifier = (node.Parent :?> ClassDeclarationSyntax).Identifier.ValueText
        let bases = 
            node.Types
            |> Seq.map (fun t-> t.Type)
            |> Seq.ofType<IdentifierNameSyntax>
            |> Seq.map (fun ins -> ins.Identifier.ValueText)
            //|> Seq.dumps (sprintf "bases on %A" parentIdentifier)
            |> List.ofSeq
        implementedInterfaces.Add(parentIdentifier,bases)
        base.VisitBaseList node

let walkFile (text:string) = 
    let tree = CSharpSyntaxTree.ParseText(text)
    let root = tree.GetRoot() :?> CompilationUnitSyntax
    let classesToBases= 
        let clsToBases = new Dictionary<string,string list>()
        let dic = ModelCollector.VisitClassInterfaces root
        dic
        //|> Seq.dumps "interfaces!"
        |> Seq.filter (fun i -> (* (i.Key,i.Value) |> dumps "keyvalue!" |> ignore; *) i.Key <> null && (i.Value |> Seq.dumps "bases" |> Seq.exists (fun v -> dumps "value check" v = "DataModelBase")))
        |> Seq.iter (fun kvp -> clsToBases.Add(kvp.Key, kvp.Value))
        clsToBases
    if classesToBases.Count > 0 then
        Some (text,root,classesToBases)
    else None



let filesToClassesToBases = 
    [ sample]
    |> Seq.choose walkFile
    |> Array.ofSeq   
    
     
let (|SimpleGetter|_|) (getter: AccessorDeclarationSyntax) =
    let nodes = getter.DescendantNodes() |> Array.ofSeq
    if nodes.Length = 3 && 
        nodes.[0] :? BlockSyntax &&
        nodes.[1] :? ReturnStatementSyntax &&
        nodes.[2] :? IdentifierNameSyntax then
        Some (
            nodes.[0] :?> BlockSyntax, 
            nodes.[1] :?> ReturnStatementSyntax,
            nodes.[2] :?> IdentifierNameSyntax
            )
    else None

type PropertyInfoB = { IsSimpleGet:bool; IsINotify:bool; Type:string; FieldName: string option; PropertyName:string; Getter:AccessorDeclarationSyntax option; Setter:AccessorDeclarationSyntax option}

let mapName s= 
        match String.trim s with
        | "" -> failwithf "name cannot be empty"
        | Id when Id.EndsWith("ID") -> Id |> String.before "ID" |> flip (+) "Id"
        | _ as s -> s

let toFType (t:string) = 
            match String.trim t with
            | nullable when nullable.Contains("?") -> nullable |> String.before "?" |> sprintf "Nullable<%s>"
            | x when x="Guid" || x = "System.Guid" -> if ScriptOptions.promoteUninitializedStructsToNullable then sprintf"(* NullableWithoutInit*) Nullable<%s>" x else x
            | x when x="Image" -> "System.Drawing.Image"
            | _ as type' -> type'
let toFull (node:#SyntaxNode) = node.ToFullString() |> String.trim
let mapToken (token:SyntaxToken)=
    match token.ValueText with
    |"==" -> "="
    |"&&"
    | _ as result -> result
let rec mapNode (memberNames:Set<string>) printDebug (node:SyntaxNode) =
    let dumps t s = if printDebug then dumps t s else s
    let dump t s = dumps t s |> ignore
    let dumpf t f s = 
        if printDebug then f s |> dumps t |> ignore
        s
    let mapNodeP node = mapNode memberNames printDebug node
    let dumpResult matchType r = if printDebug then r|> dumps (node.GetType().Name + "," + matchType + "," + node.Kind().ToString()) else r
    let mapChildren delimiter (node:#SyntaxNode) = node.ChildNodes() |> Seq.map mapNodeP |> String.join delimiter
    
    match node with
    | null -> failwithf "null node"
    | :? BlockSyntax as bs -> "BlockSyntax", mapChildren "\r\n" bs
    | :? ReturnStatementSyntax as rss -> "ReturnStatementSyntax", mapChildren  "\r\n" rss
    | :? InvocationExpressionSyntax as ies -> 
        
        let expr = mapNode memberNames printDebug ies.Expression 
        let ignoredResult = expr.StartsWith("builder.Append")
        
        //dump "ies details" <| sprintf "%A" (expr,ies.Expression.GetType().Name, ies.ArgumentList.Arguments, ies.ChildNodes())
        let arguments =ies.ArgumentList.Arguments|> Seq.map mapNodeP |> String.join ","
        "InvocationExpressionSyntax", sprintf "%s(%s)%s" expr arguments (if ignoredResult then "|> ignore" else String.Empty)
    | :? LocalDeclarationStatementSyntax as ldss ->
        let full = if ldss.Declaration.Variables.Count> 0 then Some (mapNodeP ldss.Declaration.Variables.[0]) else None
        match ldss.Declaration.Type.IsVar, ldss.Declaration.Variables.Count, full with
        | true,1, (Some x) ->"ldss var", "let mutable " + x
        | false, 1, (Some x) -> "ldss nonVar", "let mutable " + x
        | _ -> 
            dump "ldss" <| sprintf "%A" (ldss.Declaration.Type, ldss.Declaration.Variables.Count, ldss.Declaration.Variables.[0])
            "LocalDeclarationStatementSyntax", ldss |> dumpf "ldl" (fun n -> n.ChildNodes()) |> toFull |> String.replace "var " "let " |> String.trim
    | :? ElementAccessExpressionSyntax as eaes ->
        
        "ElementAccessExpressionSyntax", sprintf "%s.%s" (mapNodeP eaes.Expression) (toFull eaes.ArgumentList)
    | :? IfStatementSyntax as ifss -> 
        let statement = mapNodeP ifss.Statement
        let statements = if statement.Contains("\r\n") then "\r\n" + ScriptOptions.spacing + (String.replace "\r\n" ("\r\n" + ScriptOptions.spacing) statement) else statement 
        "IfStatementSyntax", sprintf "if %s then %s" (mapNodeP ifss.Condition) statements
    | :? PrefixUnaryExpressionSyntax as pues -> "PrefixUnaryExpressionSyntax", sprintf "not <| ( %s )" (mapNodeP pues.Operand) 
    | :? AssignmentExpressionSyntax as aes -> "AssignmentExpressionSyntax", sprintf "%s <- %s" (mapNodeP aes.Left) (mapNodeP aes.Right)
    | :? MemberAccessExpressionSyntax as maes -> 
        //dump "maes details" <| sprintf "%A" (maes.Name,maes.Expression, maes.OperatorToken)
        
        let expr = mapNodeP maes.Expression
        let token = mapToken maes.OperatorToken
        let name = mapNodeP maes.Name
        dump "maes details2" <| sprintf "%A" (expr,token,name)
        match expr, token with
        | "this","." -> "This(maes)", sprintf "%s.%s" ScriptOptions.selfIdentifier (mapName name)
        | "string","." -> "string(maes)", (sprintf "%s.%s" "String" name|> dumps "maes result")
        | x, _ when x.Contains("this.") -> "this.(maes)", x|> String.replace "this." "x."
        |_ -> "MemberAccessExpressionSyntax", sprintf "%s%s%s" expr token (mapName name)
    | :? ThisExpressionSyntax as tes -> "ThisExpressionSyntax", "x"
    | :? ArgumentSyntax as arg -> "ArgumentSyntax", mapChildren String.Empty arg 
    | :? BinaryExpressionSyntax as bes -> "BinaryExpressionSyntax", sprintf "%s %s %s" (mapNodeP bes.Left) (mapToken bes.OperatorToken) (mapNodeP bes.Right)
    | :? ExpressionStatementSyntax as ess -> "ExpressionStatementSyntax", mapChildren String.Empty ess
    | :? EqualsValueClauseSyntax as evcs -> 
            "EqualsValueClauseSyntax", sprintf "= %s" (mapNodeP evcs.Value)
    | :? VariableDeclaratorSyntax as vds ->
            //dump "vds " (sprintf "would have been %s" (toFull vds))
            "VariableDeclaratorSyntax", sprintf "%s %s" (mapToken vds.Identifier) (mapChildren String.Empty vds)
    | :? IdentifierNameSyntax as ins -> 
        if ins = null then failwithf "no identifier for ins %s" (toFull node)

        let ident = ins.Identifier
        if ident.ValueText = null then failwithf "no ValueText for ins %s" (toFull ins)
        let value = mapName ins.Identifier.ValueText
        dump "ins" <| sprintf "(parentType %A, parent %A, isVar %A, arity %A,identifier %A,kind %A)" (ins.Parent.GetType()) ins.Parent ins.IsVar ins.Arity ins.Identifier (ins.Kind())
        if memberNames.Contains(value) && value.Contains(".") = false && ins.Parent :? MemberAccessExpressionSyntax = false then
            "Ins:(propName)", sprintf "x.%s" value
        else
            if ins.Parent :? ArgumentSyntax then 
                "Identifier(argument)", sprintf "x.%s" (mapName ins.Identifier.ValueText )
            else
                "IdentifierNameSyntax", sprintf "%s" (mapName ins.Identifier.ValueText)
    | n when n.Kind() = SyntaxKind.AddExpression ->
        //if printDebug then 
            //printfn "AddExpression is type %s" (n.GetType().Name)
            //n|> dumps "mapNode:AddExpression" |> ignore
        let result = toFull n
        //if printDebug then 
            //result |> dumps "mapNode:AddExpressionText" |> ignore
        "AddExpression", result
    | _ -> "default",node |> toFull
    |> fun (t,o) -> dumpResult (sprintf "%s.%s" t <| node.Kind().ToString()) o
let mapPropertyDeclaration (prop:PropertyDeclarationSyntax) =
    let accessorCount = prop.AccessorList.Accessors.Count
    if accessorCount > 2 then failwithf "too many accessors %s" prop.Identifier.ValueText
    let tryFindAccessor k = prop.AccessorList.Accessors |> Seq.tryFind (fun a -> a.Kind() = k)
    let getter = tryFindAccessor SyntaxKind.GetAccessorDeclaration
    let setter = tryFindAccessor SyntaxKind.SetAccessorDeclaration
    let defaultResult = 
        {
            IsSimpleGet=false;
            IsINotify=prop.AccessorList.ToFullString().Contains("SetAndNotify")
            Type = prop.Type.ToFullString()
            PropertyName = prop.Identifier.ToFullString()
            FieldName = None 
            Getter=getter
            Setter=setter
        }

    match getter with
    | Some (SimpleGetter (block,ret,ident)) -> 
        {defaultResult with IsSimpleGet = true;FieldName= if ident.ToFullString().StartsWith("_") then Some <| (mapName (ident.ToFullString())) else None}
    | _ -> defaultResult
    
let getProperties (root:CompilationUnitSyntax) =
    let nodes = root.DescendantNodes() |> Array.ofSeq
    let values = [ 1; 2;3]
    let values = values|> Seq.ofType<int>
    nodes
    |> Seq.map box
    |> Seq.ofType<PropertyDeclarationSyntax> 
    |> Seq.map mapPropertyDeclaration

type FileInfoB = {File:string; Class':string; Bases: string list;Fields:FieldDeclarationSyntax list; Properties: PropertyInfoB list}
let q = 
    query{
        for (file,root,clsToBases) in filesToClassesToBases do
        for cls in clsToBases.Keys do
        let bases = clsToBases.[cls]
        
        // (* already done on line 54 *) 
        // where(Seq.contains "DataModelBase" bases)
        let properties = getProperties(root) |> List.ofSeq
        select {FileInfoB.File=file;Class'=cls;Bases =bases;Fields= root.DescendantNodes() |> Seq.ofType<FieldDeclarationSyntax> |> List.ofSeq ;Properties=properties}
    }

type FieldInfoB = {Type:string; Name:string; Initial:string option; Declaration:VariableDeclarationSyntax}
type TypeSpecification = | Type of Type | Kind of SyntaxKind
let findModel name = 
    q |> Seq.tryFind(fun fib -> fib.Class' = name ||fib.Class'.StartsWith(name))
module Declarations = 
    let (|EmptyEnumerable|NonEmpty|) (items: _ IEnumerable) =
        if Seq.isEmpty items then EmptyEnumerable else NonEmpty
    let (|SimpleInit|_|) (nodes:SyntaxNode[]) =
        let simpleKinds = [ 
            SyntaxKind.NumericLiteralExpression
            SyntaxKind.StringLiteralExpression
            SyntaxKind.NullLiteralExpression
            SyntaxKind.FalseLiteralExpression
            SyntaxKind.TrueLiteralExpression
            ]
        if nodes.Length = 4 
            && nodes.[0] :? PredefinedTypeSyntax
            && nodes.[1] :? VariableDeclaratorSyntax
            && nodes.[2] :? EqualsValueClauseSyntax
            && simpleKinds |> Seq.contains ( nodes.[3].Kind()) then
                Some (nodes.[0] :?> PredefinedTypeSyntax, nodes.[1] :?> VariableDeclaratorSyntax, nodes.[2] :?> EqualsValueClauseSyntax, nodes.[3] )
            else None
    
    let (|ArrayMatch|_|) (typeSpecifications: TypeSpecification[]) (nodes:SyntaxNode[]) =
        if nodes.Length = typeSpecifications.Length then
            let zipped = Seq.zip typeSpecifications nodes
            if zipped |> Seq.forall(fun (ts,node) -> match ts with |Type t -> t.IsAssignableFrom(node.GetType()) | Kind k-> node.Kind() = k) then
                Some()
            else None
        else
            None
            
    let (|SimplerInit|_|) (nodes:SyntaxNode[]) = 
        let simplerSpecs = [|TypeSpecification.Type(typeof<IdentifierNameSyntax>); TypeSpecification.Type(typeof<VariableDeclaratorSyntax>)  |]
        match nodes with
        | ArrayMatch simplerSpecs -> Some(nodes.[0] :?> IdentifierNameSyntax, nodes.[1] :?> VariableDeclaratorSyntax)
        | _ -> None

    let (|NullableSimplerInit|_|) (nodes:SyntaxNode[]) = 
        if nodes.Length = 3
            && nodes.[0] :? NullableTypeSyntax
            && nodes.[1] :? PredefinedTypeSyntax
            && nodes.[2] :? VariableDeclaratorSyntax then
                Some()
            else None
    let (|NullableSimpleInit|_|) (nodes:SyntaxNode[]) =
        if nodes.Length = 5
            && nodes.[0] :? NullableTypeSyntax
            && nodes.[1] :? IdentifierNameSyntax
            && nodes.[2] :? VariableDeclaratorSyntax
            && nodes.[3] :? EqualsValueClauseSyntax
            && nodes.[4].Kind() = SyntaxKind.NullLiteralExpression
            then Some ()
            else None
    let (|AutoProperty|_|) (getter'setter:AccessorDeclarationSyntax option*AccessorDeclarationSyntax option) =
        let getter,setter = fst getter'setter, snd getter'setter
        match getter,setter with
        | Some g,Some s -> 
            match g.DescendantNodes(),s.DescendantNodes() with
            | EmptyEnumerable,EmptyEnumerable -> Some ()
            | _ -> None
        | _ -> None
module FieldConversion =
    open Declarations 
    
    let convertFileFields (fileInfoB:FileInfoB) =
        let cls = fileInfoB
        let typeText = new System.Text.StringBuilder()
        let isDebugField name = name ="_AppointmentEndTime"
        let fieldNames = 
            cls.Fields 
            |> Seq.map ( fun f ->f.Declaration.Variables |> Seq.map(fun v -> v.Identifier.ValueText)) 
            |> Seq.collect id 
            |> Set.ofSeq
        Seq.iterDump fieldNames
        
        let fields = 
            cls.Fields
            //|> Seq.sortBy (fun f-> f.Declaration.Variables.Item)
            |> Seq.map (fun f -> f.Declaration.ToFullString(), f.Declaration)
            |> Seq.map ( fun (fs,vDeclaration) -> 
                if vDeclaration.Variables.Count <> 1 then failwithf "too many vars: %s" (toFull vDeclaration)
                let var = vDeclaration.Variables.[0]
                if var = null then failwithf "bad var"
                let name = var.Identifier.ValueText
                if name = null then failwith "bad var name"
                let name = mapName name
                if name = null then failwithf "failed to map name"
                let mapNode s = mapNode fieldNames (isDebugField name) s
                let initializer = if var.Initializer <> null then Some (mapNode var.Initializer) else None
                {
                    Type = toFType <| mapNode vDeclaration.Type
                    Name= name
                    Initial =  initializer
                    Declaration = vDeclaration
                } )
            |> Seq.sortBy (fun f-> f.Name)
            |> Array.ofSeq
        let toFField (memberNames:Set<string>) (fieldInfoB:FieldInfoB) = 
            let name,type',initial,vDeclaration = mapName fieldInfoB.Name, fieldInfoB.Type, fieldInfoB.Initial, fieldInfoB.Declaration
            let debug = name ="_AppointmentEndTime"
            let fDec init matchType = 

                let comments = 
                    seq {
                        if ScriptOptions.includeMatchTypeInComments then yield matchType
                        if ScriptOptions.includeOriginalInComments then yield sprintf "(%s)" (toFull vDeclaration)
                    } |> Array.ofSeq
                let comments = if Seq.isEmpty comments then String.Empty else String.Join(";",comments) |> sprintf "//%s"
                sprintf "let mutable %s : %s %s%s" name type' init comments
            let eqNullable = "=Nullable()"
            match initial with
            |Some x when x = "string.Empty"|| x= "String.Empty"-> 
                fDec "= System.String.Empty " "string.Empty-transform"
            |_ -> 
            
                match vDeclaration.DescendantNodes() |> Array.ofSeq with
                | NullableSimplerInit -> fDec eqNullable  "NullableSimplerInit"
                | NullableSimpleInit -> fDec eqNullable  "NullableSimpleInit"
                | SimplerInit(_,_) -> fDec  (if type'.Contains("Nullable") then eqNullable  else  "=null") "simpler init" //(Some (fun shouldLift -> if shouldLift then "Nullable()" else fDec "null"))
                | SimpleInit (_,_,_, literalKind) -> fDec  ("=" + toFull literalKind) "simple init"
                | _ -> fDec ("=" + (mapNode memberNames debug vDeclaration)) "default init" 

        fields 
        |> Seq.iter(fun f-> typeText.AppendLine(ScriptOptions.spacing + (toFField fieldNames f)) |> ignore)
        typeText.ToString()

module PropConversion = 
    open Declarations
    let toFProp (propertyNames:Set<string>) (pib:PropertyInfoB) = 
            let fDec getter setter matchType= 
                match getter,setter with
                | Some getter, Some setter -> sprintf "%smember x.%s //%s\r\n    with get() = %s\r\n    and set v = %s\r\n" ScriptOptions.spacing pib.PropertyName matchType getter setter
                | Some getter, None -> sprintf "%smember x.%s //%s\r\n    with get() = %s\r\n" ScriptOptions.spacing pib.PropertyName matchType getter
                | None, Some setter -> sprintf "%smember x.%s //%s\r\n    with set v = %s\r\n" ScriptOptions.spacing pib.PropertyName matchType setter
                | None,None -> sprintf "//could not declare property %s" matchType
            let simpleGet fieldName = sprintf "%s <- v" fieldName
            let iNotifyProp fieldName propName =
                sprintf "%s;x.RaisePropertyChanged(<@ x.%s @>)" (simpleGet fieldName) propName
            let printDebug = 
                //pib.PropertyName="CSZ" || 
                //pib.PropertyName = "LastNameFirstNameDOBGender" || 
                pib.PropertyName = "IsFirstEntry"

            let mapGetter name (nodes:SyntaxNode[]) = 
                //let nodes = nodes |> Seq.skip 1 |> Array.ofSeq // skip Block
                let mapNode x = mapNode propertyNames printDebug x
                nodes |> Seq.map(fun n -> n.Kind()) |> (dumps <| sprintf "getter nodes for %s" pib.PropertyName) |> ignore
                if Seq.isEmpty nodes then //autoprop
                    failwithf "map getter is not set up for empty nodes"
                let mapped = nodes |> Seq.map mapNode 
                let mapped = String.join ScriptOptions.spacing mapped
                let mapped = if mapped.Contains("\r\n") then "\r\n" + mapped else mapped
                mapped |> if printDebug then dumps "Mapnodesresult" else id

            match pib.IsINotify, pib.IsSimpleGet, pib.FieldName with
            | true,true, Some fieldName -> fDec pib.FieldName (Some (iNotifyProp fieldName pib.PropertyName)) "SimpleINotify"
            | _ -> 
                let spacing = ScriptOptions.spacing + ScriptOptions.spacing + ScriptOptions.spacing
                let mapGetter (getter:AccessorDeclarationSyntax) = 
                    let getterText = getter.ChildNodes() |> Seq.toArray |> mapGetter pib.PropertyName |> String.replace "\r\n" (sprintf "\r\n%s" spacing)
                    fDec (getterText |> Some)
                match pib.Getter,pib.Setter with
                | AutoProperty -> 
                    let value = match pib.Type with | "bool" -> "false" | _ -> "null"
                    if printDebug then 
                        printfn "AutoProperty type,value:(%s,%s)" pib.Type value
                    sprintf "%smember val %s : %s = %s with get, set\r\n" ScriptOptions.spacing pib.PropertyName pib.Type value
                | Some getter, Some setter ->
                     mapGetter getter (Some <| setter.ToFullString()) "using existing getter and setter"
                | Some getter, None ->
                    mapGetter getter None "using existing getter"
                | _ -> sprintf "  // could not generate property for %A\r\n" pib.PropertyName

    let convertProperties (fileInfoB:FileInfoB) = 
        let propNames = fileInfoB.Properties|> Seq.map(fun p -> mapName p.PropertyName) |> Set.ofSeq
        let props = fileInfoB.Properties |> Seq.map(fun p -> {p with PropertyName=mapName p.PropertyName;Type=toFType p.Type}) |> Seq.sortBy ( fun p -> p.PropertyName) 
        let f = toFProp propNames
        props |> Seq.map f

let convertFile (cls:FileInfoB) = 
    let text = "[<AllowNullLiteral>]\r\ntype " + cls.Class' + "() = \r\n  inherit FSharp.ViewModule.ViewModelBase()\r\n\r\n"
    let text = new System.Text.StringBuilder(text)
    let fieldText = FieldConversion.convertFileFields cls
    let props = PropConversion.convertProperties cls
    text.AppendLine(fieldText).AppendLine(String.Join("\r\n",props)).ToString()

let convert limit =
    let mutable items = q
    if Option.isSome limit then items <- items |> Seq.take limit.Value
    
    for cls in items do
        printfn "Starting conversion %s" cls.Class'
        let text = convertFile cls
        printfn "%s" text
let pdm'() = 
    let pdm = findModel "PatientDataModel"
    let pdm' = pdm |> Option.map convertFile
    pdm'
let apm'() =
    let apm = findModel "AppointmentDataModel"
    let apm' = apm |> Option.map convertFile
    apm'


//let getClip text = System.Windows.Forms.Clipboard.GetText()
//let setClip text = System.Windows.Forms.Clipboard.SetText(text)

//let getClassToClip f = f() |> Option.iter setClip
let pdm () = pdm'().Dump("pdm!")
let apm () = apm'().Dump("apm!")
apm()
// pdm'() |> Option.map setClip"PatientDataModel"

//open Pm.Dal

// Define your library scripting code here

