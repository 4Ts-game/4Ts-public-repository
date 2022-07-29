// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/10/13

using UnityEngine;

namespace Demigiant.DemiTools.DeUnityExtended.Components
{
    /// <summary>
    /// Controller for Shuriken <see cref="ParticleSystem"/>.
    /// Must be attached to a <see cref="ParticleSystem"/>
    /// </summary>
    [RequireComponent(typeof(ParticleSystem))]
    [AddComponentMenu("DemiTools/ParticleSystemController")]
    public class ParticleSystemController : MonoBehaviour
    {
        public ParticleSystem sys { get; private set; }
        public ParticleSystemRenderer sysRenderer { get; private set; }

        bool _initialized;
        Defaults _defaults;

        #region Unity + INIT

        void Init()
        {
            if (_initialized) return;

            _initialized = true;

            sys = this.GetComponent<ParticleSystem>();
            sysRenderer = this.GetComponent<ParticleSystemRenderer>();
            _defaults = new Defaults(sys);
        }

        void Awake()
        {
            Init();
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Initializes the controller if it wasn't already initialized, otherwise does nothing.
        /// Note the initialization happens automatically on Awake.<para/>
        /// Returns itself so it can be chained to a <code>GetComponent</code>
        /// </summary>
        public ParticleSystemController ForceInit()
        {
            Init();
            return this;
        }

        /// <summary>
        /// Sets the sortingOrder of the <see cref="ParticleSystem"/> renderer
        /// </summary>
        public void SetSortingOrder(int order)
        {
            Init();
            sysRenderer.sortingOrder = order;
        }

        /// <summary>
        /// Starts emitting and playing the <see cref="ParticleSystem"/>
        /// </summary>
        public void StartEmitting(bool withChildren = true)
        {
            Init();
            ParticleSystem.EmissionModule emission = sys.emission;
            emission.enabled = true;
            sys.Play(withChildren);
        }

        /// <summary>
        /// Stops the <see cref="ParticleSystem"/> from emitting
        /// </summary>
        public void StopEmitting(bool withChildren = true, bool andClear = false)
        {
            Init();
            sys.Stop(withChildren, andClear ? ParticleSystemStopBehavior.StopEmittingAndClear : ParticleSystemStopBehavior.StopEmitting);
        }

        /// <summary>
        /// Clears the <see cref="ParticleSystem"/>
        /// </summary>
        public void Clear(bool withChildren = true, bool andDeactivate = false)
        {
            Init();
            sys.Clear(withChildren);
            if (andDeactivate) this.gameObject.SetActive(false);
        }

        /// <summary>
        /// Pauses the <see cref="ParticleSystem"/>
        /// </summary>
        public void Pause(bool withChildren)
        {
            Init();
            sys.Pause(withChildren);
        }

        /// <summary>
        /// Resumes the <see cref="ParticleSystem"/>
        /// </summary>
        public void Resume(bool withChildren = true)
        {
            Init();
            sys.Play(withChildren);
        }

        /// <summary>
        /// Restarts the <see cref="ParticleSystem"/>
        /// </summary>
        /// <param name="withChildren">Also restart children</param>
        /// <param name="reapplyDefaultValues">Re-apply default values for the following properties:<para/>
        /// - simulationSpeed<para/>
        /// - loop<para/>
        /// - startSpeed</param>
        public void Restart(bool withChildren = true, bool reapplyDefaultValues = false)
        {
            Init();
            Clear(withChildren);
            if (reapplyDefaultValues) ReapplyDefaults();
            StartEmitting(withChildren);
        }

        /// <summary>
        /// Reapplies original values of the <see cref="ParticleSystem"/>:<para/>
        /// - simulationSpeed<para/>
        /// - loop<para/>
        /// - particles startSpeed
        /// </summary>
        public void ReapplyDefaults()
        {
            Init();
            SetLoop(_defaults.loop);
            SetSimulationSpeed(_defaults.simulationSpeed);
            SetParticlesStartSpeed(_defaults.startSpeed);
        }

        /// <summary>
        /// Immediately simulates the <see cref="ParticleSystem"/> up to the given point in time
        /// </summary>
        public void SimulateTo(float time, bool withChildren = true, bool restart = true)
        {
            Init();
            sys.Simulate(time, withChildren, restart);
        }

        /// <summary>
        /// Sets the <see cref="ParticleSystem"/> timeScale by applying the given modifier to its default <code>simulationSpeed</code>
        /// </summary>
        public void SetTimeScale(float timeScale)
        {
            Init();
            ParticleSystem.MainModule main = sys.main;
            main.simulationSpeed = _defaults.simulationSpeed * timeScale;
        }

        /// <summary>
        /// Sets the <see cref="ParticleSystem"/> simulationSpeed
        /// </summary>
        public void SetSimulationSpeed(float simulationSpeed)
        {
            Init();
            ParticleSystem.MainModule main = sys.main;
            main.simulationSpeed = simulationSpeed;
        }

        /// <summary>
        /// Activates or deactivates looping for the <see cref="ParticleSystem"/>
        /// </summary>
        public void SetLoop(bool loop)
        {
            Init();
            ParticleSystem.MainModule main = sys.main;
            main.loop = loop;
        }

        /// <summary>
        /// Sets the startColor property of the <see cref="ParticleSystem"/>
        /// </summary>
        public void SetStartColor(Color color)
        {
            Init();
            ParticleSystem.MainModule main = sys.main;
            main.startColor = new ParticleSystem.MinMaxGradient(color);
        }

        /// <summary>
        /// Sets the startSpeed property of the <see cref="ParticleSystem"/>'s particles (accepts a float too)
        /// </summary>
        public void SetParticlesStartSpeed(ParticleSystem.MinMaxCurve speed)
        {
            Init();
            ParticleSystem.MainModule main = sys.main;
            main.startSpeed = speed;
        }

        public void SetEmissionOverTimeMultiplier(float multiplier)
        {
            Init();
            ParticleSystem.EmissionModule emissionModule = sys.emission;
            emissionModule.rateOverTimeMultiplier = multiplier;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        struct Defaults
        {
            public readonly float simulationSpeed;
            public readonly ParticleSystem.MinMaxCurve startSpeed;
            public readonly bool loop;

            public Defaults(ParticleSystem system)
            {
                ParticleSystem.MainModule main = system.main;
                simulationSpeed = main.simulationSpeed;
                startSpeed = main.startSpeed;
                loop = main.loop;
            }
        }
    }
}