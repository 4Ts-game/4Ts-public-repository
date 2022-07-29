using System;
using UnityEngine;

namespace OL
{
    [Serializable]
    public class Setuppable
    {
        [SerializeField]
        protected StateConst setuppableStateValue;

        [SerializeField]
        protected int processesToBeCompleted;

        protected bool ready;
        
        protected Setuppable()
        {
           ready=false;
        }

        public enum StateConst
        {
            TO_BE_SETUPPED,
            SETUPPING,
            SETUPPED
        }

        public void CheckReady()
        {
            if (!ready)
                throw new Exception("Setuppable NewInstance lacking");
        }

        //in order to check with no exception
        public bool IsNotReady()
        {
            return !ready;
        }

        /// <summary>
        /// Forcing it to be explicityly instantiated saves you from "restart game" problems.
        /// </summary>
        /// <returns></returns>
        public static Setuppable NewInstance()
        {
            Setuppable s = new Setuppable
            {
                ready = true,

                processesToBeCompleted = 0,
                setuppableStateValue = Setuppable.StateConst.TO_BE_SETUPPED
            };
            return s;
        }

        public bool IsToBeSetupped()
        {
            CheckReady();
            return setuppableStateValue == Setuppable.StateConst.TO_BE_SETUPPED;
        }

        public bool IsSetupped()
        {
            CheckReady();
            return setuppableStateValue == Setuppable.StateConst.SETUPPED;
        }

        public bool IsSetupping()
        {
            CheckReady();
            return setuppableStateValue == Setuppable.StateConst.SETUPPING;
        }

        public void Setupping(int processes = 1)
        {
            CheckReady();
            processesToBeCompleted += processes;
            setuppableStateValue = Setuppable.StateConst.SETUPPING;
        }

        public void SetupCompleted()
        {
            CheckReady();
            processesToBeCompleted--;
            if (processesToBeCompleted <= 0)
                setuppableStateValue = Setuppable.StateConst.SETUPPED;
        }

        public bool NotSetuppedNorSetupping()
        {
            return !IsSetupped() && !IsSetupping();
        }
    }
    
    [Serializable]
    public class State : Setuppable
    {
        [SerializeField]
        StateProgressConst stateValue;

        enum StateProgressConst
        {
            WAITING_SETUP,
            RUNNING,
            PAUSED,
            ENDED
        }

        private State()
        {
        }

        public new static State NewInstance()
        {
            State s = new State
            {
                ready = true,
                processesToBeCompleted = 0,
                setuppableStateValue = StateConst.TO_BE_SETUPPED,
                stateValue = StateProgressConst.WAITING_SETUP
            };
            return s;
        }

        public bool IsRunning()
        {
            CheckReady();
            return stateValue == StateProgressConst.RUNNING;
        }

        public bool IsEnded()
        {
            CheckReady();
            return stateValue == StateProgressConst.ENDED;
        }

        public bool IsPaused()
        {
            CheckReady();
            return stateValue == StateProgressConst.PAUSED;
        }

        public void Run()
        {
            CheckReady();
            if (setuppableStateValue != StateConst.SETUPPED)
                throw new Exception("Not setupped");
            stateValue = StateProgressConst.RUNNING;
        }

        public void Pause()
        {
            CheckReady();
            if (setuppableStateValue != StateConst.SETUPPED)
                throw new Exception("Not setupped");
            stateValue = StateProgressConst.PAUSED;
        }

        public void End()
        {
            CheckReady();
            if (setuppableStateValue != StateConst.SETUPPED)
                throw new Exception("Not setupped");
            if (stateValue != StateProgressConst.RUNNING && stateValue != StateProgressConst.PAUSED)
                throw new Exception("Not running");
            stateValue = StateProgressConst.ENDED;
        }

        public bool IsReadyToRun()
        {
            CheckReady();
            return IsSetupped() && !IsRunning() && !IsEnded();
        }

        public override string ToString()
        {
            return "Setup state "+setuppableStateValue+ " Progress " + stateValue;
        }
    }


}