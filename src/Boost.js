import React from 'react'

const Boost = () => {
  return (
    <div>
        <button style={{
          '--bg': '#e74c3c',
          '--text-color': '#fff',
          position: 'relative',
          width: '150px',
          border: 'none',
          background: 'var(--bg)',
          color: 'var(--text-color)',
          padding: '1em',
          fontWeight: 'bold',
          textTransform: 'uppercase',
          transition: '0.2s',
          borderRadius: '5px',
          opacity: '0.8',
          letterSpacing: '1px',
          boxShadow: '#c0392b 0px 7px 2px, #000 0px 8px 5px',
        }}>
            Boost
        </button>
    </div>
  )
}

export default Boost