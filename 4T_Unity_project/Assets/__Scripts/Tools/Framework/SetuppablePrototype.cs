using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using UnityEngine;

namespace OL
{

    /// <summary>
    /// ORIGINAL:
    /// Convenience way to create objects in the hierarchy that are a bit like prefabs, except that they
    /// aren't. They exist in the scene, immediately deactivate themselves, then provide an easy way to
    /// instantiate a copy in the same place in the hierarchy.
    /// If you call ReturnToPool() once you're done with the instance, then it's returned
    /// to the original prototype to be reused if necessary instead of creating a brand new GameObject.
    /// FROM https://github.com/inkle/prototype/blob/master/Prototype.cs
    /// Changed with setup
    /// </summary>
    public class SetuppablePrototype : MonoBehaviour
    {
        public event Action OnReturnToPool;

        public bool isOriginalPrototype
        {
            get
            {
                return _originalSetuppablePrototype == null;
            }
        }

        public SetuppablePrototype OriginalSetuppablePrototype
        {
            get
            {
                return _originalSetuppablePrototype;
            }
        }

        void Start()
        {
            if (isOriginalPrototype)
                this.gameObject.SetActive(false);
        }

        void OnDestroy()
        {
            if (_instancePool != null)
                foreach (var inst in _instancePool)
                    Destroy(inst);
        }

        public T Instantiate<T>() where T : Component
        {
            SetuppablePrototype instance = null;

            // Re-use instance from pool
            if (_instancePool != null && _instancePool.Count > 0)
            {
                var instanceIdx = _instancePool.Count - 1;
                instance = _instancePool[instanceIdx];
                _instancePool.RemoveAt(instanceIdx);
            }

            // Instantiate fresh instance
            else
            {
                instance = UnityEngine.Object.Instantiate(this);

                instance.transform.SetParent(transform.parent, worldPositionStays: false);

                var protoRT = transform as RectTransform;
                if (protoRT)
                {
                    var instRT = instance.transform as RectTransform;
                    instRT.anchorMin = protoRT.anchorMin;
                    instRT.anchorMax = protoRT.anchorMax;
                    instRT.pivot = protoRT.pivot;
                    instRT.sizeDelta = protoRT.sizeDelta;
                }

                instance.transform.localPosition = transform.localPosition;
                instance.transform.localRotation = transform.localRotation;
                instance.transform.localScale = transform.localScale;

                instance._originalSetuppablePrototype = this;
            }

            T instanceT = instance.GetComponent<T>();
            MethodInfo methodInfo = typeof(T).GetMethod("Setup");
            methodInfo.Invoke(instanceT, null);
            instance.gameObject.SetActive(true);
            return instanceT;
        }

        public void ReturnToPool()
        {
            if (isOriginalPrototype)
            {
                Debug.LogError("Can't return to pool because the original prototype doesn't exist. Is this prototype the original?");
                Destroy(gameObject);
                return;
            }

            _originalSetuppablePrototype.AddToPool(this);
        }

        public T GetOriginal<T>()
        {
            if (isOriginalPrototype)
                return GetComponent<T>();
            else
                return OriginalSetuppablePrototype.GetComponent<T>();
        }

        void AddToPool(SetuppablePrototype instanceSetuppablePrototype)
        {
            if (!isOriginalPrototype)
                Debug.LogError("Adding " + instanceSetuppablePrototype.name + " to prototype pool of " + this.name + " but this appears to be an instance itself?");

            instanceSetuppablePrototype.gameObject.SetActive(false);

            if (_instancePool == null) _instancePool = new List<SetuppablePrototype>();
            _instancePool.Add(instanceSetuppablePrototype);

            if (instanceSetuppablePrototype.OnReturnToPool != null)
                instanceSetuppablePrototype.OnReturnToPool();
        }

        SetuppablePrototype _originalSetuppablePrototype;

        List<SetuppablePrototype> _instancePool;
    }


}
