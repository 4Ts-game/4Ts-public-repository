// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/16

using System;
using System.Collections.Generic;
using DG.Tweening.Timeline.Core;
using UnityEngine;

namespace DG.Tweening.Timeline
{
    /// <summary>
    /// This is meant as a utility Component that contains list of <see cref="DOVisualSequence"/> elements.
    /// If you prefer to control <see cref="DOVisualSequence"/> elements directly (I do) you can do that
    /// by directly adding serialized <see cref="DOVisualSequence"/> elements to your own Components
    /// </summary>
    public class DOVisualSequenceCollection : MonoBehaviour
    {
        #region Serialized
#pragma warning disable 0649

        public DOVisualSequence[] sequences = new []{ new DOVisualSequence(Guid.NewGuid().ToString()) };

#pragma warning restore 0649
        #endregion

        #region Unity

        void Start()
        {
            int len = sequences.Length;
            if (DOVisualSequenceSettings.I.debugLogs) {
                DOLog.Normal(string.Format("DOVisualSequenceCollection <color=#d568e3>{0}</color> : Startup, iterating through {1} DOVisualSequences", this.name, len), this);
            }
            for (int i = 0; i < len; ++i) {
                if (sequences[i].isActive) sequences[i].GenerateTween();
            }
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Returns the first <see cref="DOVisualSequence"/> element in this object that has the given name, or NULL if none was found.<para/>
        /// NOTE: in case you have multiple <see cref="DOVisualSequence"/> elements with the same name and want to return them all,
        /// use <see cref="FindSequencesByName(string)"/> instead.
        /// </summary>
        public DOVisualSequence FindSequenceByName(string sequenceName)
        {
            int len = sequences.Length;
            for (int i = 0; i < len; ++i) {
                if (sequences[i].name == sequenceName) return sequences[i];
            }
            return null;
        }

        /// <summary>
        /// Returns a list of this object's <see cref="DOVisualSequence"/> elements that have the given name, or NULL if none was found.
        /// </summary>
        public List<DOVisualSequence> FindSequencesByName(string sequenceName)
        {
            List<DOVisualSequence> result = null;
            FindSequencesByName(sequenceName, ref result);
            return result;
        }
        /// <summary>
        /// Adds to the given list all of this object's <see cref="DOVisualSequence"/> elements that have the given name.<para/>
        /// Note that this method doesn't clear <see cref="fillList"/> before adding elements,
        /// nor initializes it if no corresponding <see cref="DOVisualSequence"/> is found
        /// (which means that if you pass a NULL <see cref="fillList"/> it will remain NULL in case there are no matches).
        /// </summary>
        public void FindSequencesByName(string sequenceName, ref List<DOVisualSequence> fillList)
        {
            int len = sequences.Length;
            for (int i = 0; i < len; ++i) {
                if (sequences[i].name != sequenceName) continue;
                if (fillList == null) fillList = new List<DOVisualSequence>();
                fillList.Add(sequences[i]);
            }
        }

        #endregion
    }
}